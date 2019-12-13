#!/usr/bin/python
# -------------------------------------------------------------------
# - NAME:        GEFS.py
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-11
# -------------------------------------------------------------------
# - DESCRIPTION: Quick and dirty py2.7 script to download some data
#                for delta airlines. Requires wgrib2 for the spatial
#                subsetting. 
# -------------------------------------------------------------------
# - EDITORIAL:   2018-10-11, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-10-11 23:03 on marvin
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# -------------------------------------------------------------------
def bar():
    """bar()

    Simply prints a line of "-" on stdout.
    """
    log.info("{:s}".format("".join(["-"]*70)))

# -------------------------------------------------------------------
# -------------------------------------------------------------------
def get_file_names(filedir, baseurl, date, mem, step):
    """get_file_names(filedir, baseurl, date, mem, step)

    Generates the file names (remote and local).

    Parameters
    ----------
    filedir : str
        name of the directory where to create the file "local"
    baseurl : str
        base url, used to format the data (can contain %Y%m%d or similar)
    date : datetime.datetime
        defines model initialization date and time
    mem : int
        member number
    step : int
        forecast step (in hours)

    Returns
    -------
    Returns a dict with three entries for the indexfile ("idx"), the grib file ("grib"),
    and the local file name ("local"), plus the file name of the local subset. Only
    used if subset is defined and wgrib2 is available (see main script).
    """
    import os
    import datetime as dt
    from numpy import int64
    if not isinstance(mem, int) and not isinstance(mem, int64):
        raise ValueError("member has to be an integer (get_index_file function)")
    if not isinstance(step, int) and not isinstance(step, int64):
        raise ValueError("step has to be an integer (get_index_file function)")

    # Create URL
    yyyy = int(date.strftime("%Y"))
    mm   = int(date.strftime("%m"))
    dd   = int(date.strftime("%d"))
    HH   = int(date.strftime("%H"))
    gribfile = os.path.join(baseurl.format(yyyy, mm, dd, HH),
                            "ge{:s}{:02d}.t{:s}z.pgrb2f{:s}".format(
                            "c" if mem == 0 else "p", mem, date.strftime("%H"),
                            "{:02d}".format(step) if step < 100 else "{:03d}".format(step)))
    local    = date.strftime("GEFS_%Y%m%d_%H00") + "_{:02d}_f{:03d}.grb2".format(mem, step)
    subset   = date.strftime("GEFS_%Y%m%d_%H00") + "_{:02d}_f{:03d}_subset.grb2".format(mem, step)
    return {"grib"   : gribfile,
            "idx"    : "{:s}.idx".format(gribfile), 
            "local"  : os.path.join(filedir, local),
            "subset" : os.path.join(filedir, subset)}


# -------------------------------------------------------------------
# -------------------------------------------------------------------
class idx_entry(object):
    def __init__(self, args):
        """idx_entry(args)

        A small helper class to handle index entries.
        
        Parameters
        ----------
        args : list
            list with three entries (bytes start, param name, param level)
        """
        self._byte_start = int(args[0])
        self._var        = str(args[1])
        self._lev        = str(args[2])
        self._byte_end   = False

    def add_end_byte(self, x):
        """add_end_byte(x)

        Appends the ending byte.
        """
        self._byte_end = x

    def end_byte(self):
        """end_byte()

        Returns end byte.
        """
        try:
            x = getattr(self, "_byte_end")
        except:
            raise Error("whoops, _byte_end attribute not found.")
        return x 

    def start_byte(self):
        """start_byte()

        Returns start byte.
        """
        try:
            x = getattr(self, "_byte_start")
        except:
            raise Error("whoops, _byte_start attribute not found.")
        return x 

    def key(self):
        """key()

        Returns
        -------
        Returns a character string "<param name>:<param level>".
        """
        try:
            var = getattr(self, "_var")
            lev = getattr(self, "_lev")
        except Exception as e:
            raise Exception(e)

        return "{:s}:{:s}".format(var,lev)

    def range(self):
        """range()

        Returns
        -------
        Returns the byte range for curl.
        """
        try:
            start = getattr(self, "_byte_start")
            end   = getattr(self, "_byte_end")
        except Exception as e:
            raise Exception(e)
        end = "" if end is None else "{:d}".format(end)

        return "{:d}-{:s}".format(start, end)

    def __repr__(self):
        if isinstance(self._byte_end, bool):
            end = "UNKNOWN"
        elif self._byte_end is None:
            end = "end of file"
        else:
            end = "{:d}".format(self._byte_end)
        return "IDX ENTRY: {:10d}-{:>10s}, '{:s}'".format(self._byte_start,
                end, self.key())

# -------------------------------------------------------------------
# -------------------------------------------------------------------
def parse_index_file(idxfile, params):
    """parse_index_file(idxfile, config)
 
    Downloading and parsing the grib index file, extracts the
    bytes of the required fields/variables.

    Parameters
    ----------
    idxfile : str
        url to the index file
    params : dict
        contains the config of the requested parameters

    Returns
    -------
    Things ... (tdb)
    """

    if sys.version_info.major < 3:
        import urllib2 as urllib
    else:
        import urllib.request as urllib

    try:
        req  = urllib.Request(idxfile)
        data = urllib.urlopen(req).read().decode("utf-8").split("\n")
    except Exception as e:
        log.warning("[!] Problems reading index file\n    {:s}\n    ... return None".format(idxfile))
        sys.exit(3)
        return None

    # List to store the required index message information
    idx_entries = []

    # Parsing data (extracting message starting byte,
    # variable name, and variable level)
    import re
    comp = re.compile("^\d+:(\d+):d=\d{10}:([^:.*]*):([^:.*]*)")
    byte = 1 # initial byte
    for line in data:
        if len(line) == 0: continue
        mtch = re.findall(comp, line)
        if not mtch:
            raise Exception("whoops, pattern mismatch \"{:s}\"".format(line))
        # Else crate the variable hash
        idx_entries.append(idx_entry(mtch[0]))

    # Now we know where the message start (bytes), but we do not
    # know where they end. Append this information.
    for k in range(0, len(idx_entries)):
        if (k + 1) == len(idx_entries):
            idx_entries[k].add_end_byte(None)
        else:
            idx_entries[k].add_end_byte(idx_entries[k+1].start_byte() - 1)

    # Go trough the entries to find the messages we request for.
    res = []
    params = [val for key,val in params.items()]
    for x in idx_entries:
        if x.key() in params: res.append(x.range())
        
    # Return ranges to be downloaded
    return res

# -------------------------------------------------------------------
def download_range(grib, local, range):
    log.info("Downloading data for {:s}".format(local))
    import sys
    if sys.version_info.major < 3:
        import urllib2 as urllib
    else:
        import urllib.request as urllib

    try:
        req = urllib.Request(grib)
        req.add_header("Range", "bytes={:s}".format(",".join(range)))
        resp = urllib.urlopen(req)
    except:
        log.error("[!] Problems downloading the data.\n    Return None, trying to continue ...")
        sys.exit(3)
        return None

    with open(local, "wb") as fid: fid.write(resp.read())
    return True

# -------------------------------------------------------------------
# Read config file
# -------------------------------------------------------------------
def get_config():

    import os
    import sys
    import socket
    if sys.version_info.major < 3:
        from ConfigParser import ConfigParser
    else:
        from configparser import ConfigParser
    CNF = ConfigParser()

    # Hostname specific config?
    host = socket.gethostname()
    if os.path.isfile("{:s}_config.conf".format(host)):
        log.info("Reading custom config file for \"{:s}\"".format(host))
        CNF.read("{:s}_config.conf".format(host))
    else:
        log.info("Reading default config.conf file")
        if not os.path.isfile("config.conf"):
            raise Exception("Cannot find config file config.conf")
        CNF.read("config.conf")

    # Output directory
    outdir = CNF.get("main", "outdir")
    # Base url
    url    = CNF.get("main", "url")

    # Check if the user requests a subset.
    subset = CNF.getboolean("subset", "use")
    if subset:
        subset = {"W": CNF.getfloat("subset", "W"), "E": CNF.getfloat("subset", "E"),
                  "S": CNF.getfloat("subset", "S"), "N": CNF.getfloat("subset", "N")}
    else:
        subset = None

    # read parameter config
    # [param xxx]
    # name  = ...
    # level = ...
    # xxx: identifier, currently not really used except to set up the dictionary (char)
    # name: name of the varaible according to the grib index file (char)
    # level: level of the variable according to the grib index file (char)
    from re import match
    sections = CNF.sections()
    params = dict()
    for sec in sections:
        mtch = match("^param\s+(.*)$", sec)
        if not mtch: continue
        # Checking items
        items = dict()
        for rec in CNF.items(sec): items[rec[0]] = rec[1]
        if not "name" in items.keys() or not "level" in items.keys(): continue
        params[mtch.group(1)] = "{:s}:{:s}".format(items["name"], items["level"])
        log.info("Found parameter specification for {:s}".format(params[mtch.group(1)]))

    return [outdir, url, subset, params]


# -------------------------------------------------------------------
# Main script
# -------------------------------------------------------------------
if __name__ == "__main__":

    import logging as log
    log.basicConfig(level = log.DEBUG)

    ##### Config
    ####outdir = "data"
    ####baseurl = "http://nomads.ncep.noaa.gov/pub/data/nccf/com/gens/prod/gefs.%Y%m%d/%H/pgrb2/"


    ###### List of the required parameters. Check the index file
    ###### to see the available parameters. Always <param>:<level> where
    ###### <param> and <level> are the strings as in the grib index file.
    #####params = ["GUST:surface",
    #####          "UGRD:10 m above ground",  "VGRD:10 m above ground",
    #####          "UGRD:80 m above ground",  "VGRD:80 m above ground",
    #####          "UGRD:100 m above ground", "VGRD:100 m above ground"]

    # Import some required packages
    import sys, os, re
    import argparse, sys
    import datetime as dt
    import numpy as np
    import distutils.spawn
    import subprocess as sub

    # Parsing input args
    parser = argparse.ArgumentParser(description="Download some GEFS data")
    parser.add_argument("--date","-d", type = str,
               help = "Model initialization date. Format has to be YYYY-mm-dd!")
    parser.add_argument("--runhour","-r", type = int,
               help = "Model initialization hour, 0/6/12/18, integer.")
    args = vars(parser.parse_args())

    # Checking args
    if args["runhour"] is None or args["date"] is None:
        parser.print_usage(); sys.exit(9)
    if not re.match("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", args["date"]):
        parser.print_usage()
        raise ValueError("wrong format for input -d/--date")
    if not args["runhour"] in [0, 6, 12, 18]:
        parser.print_usage()
        raise ValueError("wrong input for -r/--runhour, has to be 0/6/12/18")

    # Crate date arg
    date   = dt.datetime.strptime("{:s} {:02d}:00".format(args["date"], args["runhour"]),
                                  "%Y-%m-%d %H:%M")

    # Steps/members. The +1 is required to get the required sequence!
    steps   = np.arange(6, 300+1, 6, dtype = int)
    members = np.arange(0,  20+1, 1, dtype = int)

    # Reading config
    outdir, baseurl, subset, params = get_config()

    bar()
    log.info("Downloading steps:\n  {:s}".format(", ".join(["{:d}".format(x) for x in steps])))
    log.info("Downloading members:\n  {:s}".format(", ".join(["{:d}".format(x) for x in members])))
    log.info("For date/model initialization\n  {:s}".format(date.strftime("%Y-%m-%d %H:%M UTC")))
    log.info("Base url:\n  {:s}".format(date.strftime(baseurl)))

    # Looping over the different members first
    for mem in members:
        # Looping over forecast lead times
        for step in steps:
            bar()
            log.info("Processing +{:03d}h forecast, member {:02d}".format(step, mem))

            # Specify and create output directory if necessary
            filedir = "{:s}/{:s}".format(outdir, date.strftime("%Y%m%d%H%M"))
            if not os.path.isdir(filedir):
                try:
                    os.makedirs(filedir)
                except:
                    raise Exception("Cannot create directory {:s}!".format(filedir))

            # Getting file names
            files = get_file_names(filedir, baseurl, date, mem, step)
            if os.path.isfile(files["subset"]):
                print("- Local subset exists, skip")
                bar()
                continue
            if os.path.isfile(files["local"]):
                print("- Local file exists, skip")
                bar()
                continue


            # Else start download
            log.debug("- Grib file:   {:s}".format(files["grib"]))
            log.debug("- Index file:  {:s}".format(files["idx"]))
            log.debug("- Local file:  {:s}".format(files["local"]))
            log.debug("- Subset file: {:s}".format(files["subset"]))

            # Read/parse index file (if possible)
            required = parse_index_file(files["idx"], params)

            # If no messages found: continue
            if required is None: continue
            if len(required) == 0: continue

            # Downloading the data
            download_range(files["grib"], files["local"], required)

            # If wgrib2 exists: crate subset (small_grib)
            check = distutils.spawn.find_executable("wgrib2")
            if not check is None and not subset is None:
                WE  = "{:.2f}:{:.2f}".format(subset["W"], subset["E"])
                SN  = "{:.2f}:{:.2f}".format(subset["S"], subset["N"])
                cmd = ["wgrib2", "-g2clib", "0", files["local"], "-small_grib", WE, SN, files["subset"]] 
                log.info("- Subsetting: {:s}".format(" ".join(cmd)))
                p = sub.Popen(cmd, stdout = sub.PIPE, stderr = sub.PIPE) 
                out,err = p.communicate()

                if p.returncode == 0:
                    log.info("- Subset created, delete global file")
                    os.remove(files["local"])
                else:
                    log.info("[!] Problem with subset, do not delete global grib2 file.")


            bar()
























