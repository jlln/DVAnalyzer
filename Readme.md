THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

This application is based around the ImageJ Java library. 
Schneider, C.A., Rasband, W.S., Eliceiri, K.W. "NIH Image to ImageJ: 25 years of image analysis". Nature Methods 9, 671-675, 2012.

This application performs automatic quantification of nuclear features in three-channel 3D tiff images produced by widefield deconvolution fluorescence microscopy. It accepts as input a directory, containing subdirectories containing tiff images. Each subdirectory corresponds to an experimental condition, and the application uses the names of the subdirectories to label the results.

The tiff files should be in red,green,blue channel order. The blue channel is used to prepare masks of the nuclei, which are then traced through the stack to produce three-dimensional representations of the nuclei. Because the nuclei can potentially lie in different focal planes, the focussed slices of each nuclei are then identified, and the unfocussed slices of each nucleus are discarded.

Subnuclear objects are thresholded using a method derived from the Stable count thresholding technique described in 
Russell RA, Adams NM, Stephens DA, Batty E, Jensen K, Freemont PS. Segmentation of Fluorescence Microscopy Images for Quantitative Analysis of Cell Nuclear Architecture. Biophysical Journal 96, 3379-3389, 2009. 

The total area of each nucleus is calculated as the sum of the areas of its constituent slices. Measurements of the Pearson's correlation,  staining intensity, object counts, object sizes, and object position are made for each slice in the nucleus, and a weighted average is prepared according to the area of each slice. 

The output is written to a csv file in the working directory, with each row corresponding to a single cell.





