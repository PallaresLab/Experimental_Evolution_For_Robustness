from utils import*
import sys
input_file = sys.argv[1]
output_file = sys.argv[2]
df = dlib_xml_to_pandas(input_file)
df.to_csv(output_file)
quit()

