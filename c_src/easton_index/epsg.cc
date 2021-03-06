// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include "easton.hh"
#include "epsg.hh"


const char* EASTON_EPSG_TABLE[EASTON_EPSG_COUNT] = {
    "GGRS87.LL",
    "LL-ATS77",
    "KKJb.LL",
    "RT90-3/7Pa.LL",
    NULL,
    NULL,
    "TETE/b.LL",
    NULL,
    NULL,
    "Moznet.LL",
    "Indian1960/E.LL",
    "Final1958.LL",
    "Estonia92b.LL",
    "PDOSurvey93b.LL",
    "LL-OLDHI",
    NULL,
    NULL,
    NULL,
    "PRVI.LL",
    NULL,
    "Israel.LL",
    "Locodjo1965.LL",
    "Abidjan1987.LL",
    "Kalianpur1937.LL",
    "Kalianpur1962.LL",
    "Kalianpur1975.LL",
    "Hanoi1972.LL",
    "Hartebeesthoek94.LL",
    "CH1903/GSB.LL",
    "CH1903Plus_1.LL",
    "CHTRF95.LL",
    "LL-HPGN",
    "Rassadiran_1.LL",
    "Europ50/1977.LL",
    "Dabola1981.LL",
    "CzechJTSK/5b.LL",
    NULL,
    "NAPARIMA.LL",
    "ELD1979.LL",
    NULL,
    "PampaCastillo.LL",
    NULL,
    "YemenNtl96.LL",
    "SouthYemen_1.LL",
    "Bissau_1.LL",
    "Korean95.LL",
    "NZGD2000.LL",
    "Accra1929.LL",
    "Samoa1962.LL",
    "GRSSA.LL",
    "LL-RGF93",
    NULL,
    "IRENET95.LL",
    NULL,
    "SierraLeone1968.LL",
    "Antarctic98.LL",
    NULL,
    "Pulkovo42/83b.LL",
    "Pulkovo42/58b.LL",
    "Estonia97.LL",
    "Luxembourg30b.LL",
    "OBSRV66.LL",
    "AZORES.LL",
    "SAOBRAZ.LL",
    NULL,
    NULL,
    NULL,
    "OSNI52/b.LL",
    "REGVEN.LL",
    "PGA98.LL",
    NULL,
    "Douala1948.LL",
    "Manoca1962.LL",
    "Qornoq1927.LL",
    "Scoresbysund52b.LL",
    "Ammassalik58b.LL",
    NULL,
    NULL,
    NULL,
    NULL,
    "Adindan.LL",
    "LL-ASTRLA66-Grid",
    "LL-AGD84-Grid",
    "AinElAbd.LL",
    "Afgooye.LL",
    NULL,
    "Lisbon37/b.LL",
    "Aratu_1.LL",
    "Arc1950.LL/01",
    "Arc1960.LL",
    "Batavia_1.LL",
    "Barbados1938.LL",
    NULL,
    "Beijing1954/a.LL",
    NULL,
    "Bermuda.LL",
    NULL,
    "Bogota.LL",
    "BukitRimpah_1.LL",
    "Camacupa_1.LL",
    "Campo.LL",
    "Cape-1.LL",
    "Carthage.LL",
    "Chua.LL",
    "Corrego.LL",
    NULL,
    "DeirEzZor_2.LL",
    NULL,
    "Old-Egyp.LL",
    "LL-ERP50",
    "Europ87/a.LL",
    "Fahud_1.LL",
    NULL,
    NULL,
    NULL,
    "HuTzuShan_1.LL",
    "HD72/7Pa.LL",
    "Indonesian1974.LL",
    "Indian1954.LL",
    "Indian75/E.LL",
    NULL,
    "Jamaica1969.LL",
    NULL,
    "Kandawala.LL",
    "Kertau48.LL",
    "KuwaitOil.LL",
    "LaCanoa/E.LL",
    "LL-PSAD56",
    NULL,
    "Leigon_1.LL",
    "Liberia.LL",
    NULL,
    "Luzon.LL",
    "HitoXVIII63b.LL",
    "HeratNorth_1.LL",
    "Mahe1971.LL",
    "Makassar/E.LL",
    "LL-ETRF89",
    "Malongo1987.LL",
    NULL,
    "Merchich/01",
    "Massawa.LL",
    "Minna.LL",
    "Mhast/gc.LL",
    "ROME1940.LL",
    "Mporaloko_1.LL",
    "LL27",
    "MICHIGAN.LL",
    "LL83",
    "NHRWN-O.LL",
    "Naparima1972.LL",
    "LL-GD1949-Grid",
    "NGO48b.LL",
    "Datum73b.LL",
    "NTF.LL",
    NULL,
    "OSGB.LL",
    NULL,
    NULL,
    NULL,
    "Palestine23a.LL",
    "PointeNoire60.LL",
    "LL-GDA94",
    "Pulkovo42.LL",
    "Qatar1974.LL",
    "Qatar48.LL",
    NULL,
    NULL,
    "Amersfoort/a.LL",
    NULL,
    "SAD69.LL",
    "Sapper.LL",
    "Schwarzk.LL",
    NULL,
    NULL,
    NULL,
    "Tananarive1925.LL",
    "TIMBALAI.LL",
    "TM1965/b.LL",
    NULL,
    "Tokyo",
    "Trinidad1903.LL",
    NULL,
    "Voirol1875_1.LL",
    NULL,
    NULL,
    "NordSahara1959.LL",
    NULL,
    "Yacare/E.LL",
    NULL,
    "Zanderij.LL",
    "MGI-AT/a.LL",
    "Belge72/b.LL",
    "DHDN/3.LL",
    "Conakry1905.LL",
    "DealulPiscului1933.LL",
    NULL,
    "NtlGeodeticNet.LL",
    "KuwaitUtility.LL",
    NULL,
    NULL,
    "LL72",
    NULL,
    "WGS72-TBE/b.LL",
    NULL,
    "LL84",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    "PA-S",
    "NY-LI",
    "SD83-NF",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    "Antilles91/1.LL",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    "Antigua1943.LL",
    "Dominica1945.LL",
    "Grenada1953.LL",
    "Montserrat1958.LL",
    "StKitts1955.LL",
    "StLucia1955.LL",
    "StVincent1945.LL",
    NULL,
    NULL,
    "Xian80.LL",
    "HongKong80b.LL",
    "LL-JGD2K-7P",
    "GunungSegara.LL",
    "QatarNtl95b.LL",
    "MADEIRA.LL",
    "Selvagem.LL",
    "LL-CSRS",
    NULL,
    "SWEREF99.LL",
    "Point1958.LL",
    "FortMarigot_1.LL",
    "Guadeloupe48.LL",
    "CSG1967.LL",
    "Guyane95a.LL",
    "Martinique38.LL",
    "REUNION.LL",
    "Reunion92.LL",
    "Tahiti52.LL",
    "Tahaa54.LL",
    "IGN72/NukuHiva.LL",
    NULL,
    "Combani1950.LL",
    "IGN56/Lifou.LL",
    NULL,
    "ST87Ouvea.LL",
    "Petrels1972.LL",
    "Perroud1950.LL",
    "Miquelon1950.LL",
    "MOP1978a.LL",
    NULL,
    "IGN53/Mare.a.LL",
    "ST84IlePins/a.LL",
    "ST71Belep/b.LL",
    "Noumea74.LL",
    "Caledonie91.LL",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    "Reykjavik.LL",
    "HJORSEY.LL",
    "IslandsNet1993.LL",
    "Helle1954a.LL",
    "Latvia1992.LL",
    "IGN72/GrandeTerre.LL",
    "PortoSanto95.LL",
    "AzoresEast1995.LL",
    "AzoresCntrl1995.LL",
    "Lisbon1890_1.LL",
    "IraqKuwait1992.LL",
    "EUROP79.LL",
    "Lithuania94.LL",
    "IGM1995.LL",
    NULL,
    "CHATHAM.LL",
    "Chatham1979a.LL",
    "SRG-SA/2000.LL",
    "GUAM63.LL",
    NULL,
    NULL,
    "Lao97.LL",
    "Jouik61.LL",
    "Nouakchott65.LL",
    NULL,
    "Gulshan303.LL",
    "PRS92/03.LL",
    "Gan70.LL",
    NULL,
    "MarcoGNR.LL",
    "RGP-Francaise/a.LL",
    "FatuIva/72a.LL",
    "IGN63/Hiva Oa/a.LL",
    NULL,
    "Moorea87a.LL",
    "Maupiti1983.LL",
    "Nakhl-eGhanem.LL",
    NULL,
    NULL,
    NULL,
    NULL,
    "KERGUELN.LL",
    "LePouce34.LL",
    NULL,
    "CongoBelge55.LL",
    "Mauritania1999.LL",
    NULL,
    NULL,
    NULL,
    NULL,
    "Tern61.LL",
    "CocosIsl1965.LL",
    "IwoJima45.LL",
    "StHelena71.LL",
    "Marcus52.LL",
    "ASCENSN.LL",
    "AyabelleLH.LL",
    "BELLEVUE.LL",
    "CampAreaAstro_1.LL",
    "PhoenixIs66.LL",
    "CANAVRL.LL",
    "Solomon68.LL",
    "EASTER.LL",
    "Fiji1986a.LL",
    "Fiji56.LL",
    "SouthGeorgia68.LL",
    "GrandCayman59.LL",
    "DiegoGarcia69.LL",
    "JHNSTN.LL",
    "LittleCayman61.LL",
    "MIDWAY.LL",
    "CANARY.LL",
    "PITCAIRN.LL",
    "Santo65.LL",
    "VITI.LL",
    "Marshalls60.LL",
    "WakeIs1952.LL",
    "TRISTAN.LL",
    "Kusaie51.LL",
    "DeceptionIsland_1.LL",
    "Korean2000.LL",
    "HONGKONG.LL",
    "HongKong63/1967.LL",
    "ParametropZemp1990a.LL",
    NULL,
    NULL,
    "Karbala79/P.LL",
    NULL,
    NULL,
    NULL,
    "Greenland1996.LL",
    "VanuaLv1915.LL",
    "RGN-Caledonie/91-93.LL",
    NULL,
    NULL,
    "VitiLevu12.LL",
    NULL,
    "Libyan2006_1.LL",
    "Nasional1995.LL",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    "MGI-AT/Fa.LL/a",
    NULL,
    "LL-NTF-Grid",
};


const char*
easton_epsg_lookup(uint64_t code)
{
    if(code < EASTON_EPSG_MIN || code > EASTON_EPSG_MAX) {
        return NULL;
    }

    return EASTON_EPSG_TABLE[code - EASTON_EPSG_MIN];
}

