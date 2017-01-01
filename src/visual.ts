/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
module powerbi.extensibility.visual {

	//PBI_TEMPLATE_1: interface definition blocks
	interface Visual_settings_prepocessing_params {
		show: boolean;
		scaleData: boolean;
		applyPCA: boolean;
	}

	interface Visual_settings_clusterNum_params {
		granularityParameterType: string;
		percentile: number;
		eps: number;
		autoParamMinPts: boolean;
		minPtsParam: number;
	}

	interface Visual_settings_viz_params {
		show: boolean;
		drawEllipse: boolean;
		drawConvexHull: boolean;
		drawCenters: boolean;
		percentile: number;
		weight: number;
	}

	interface Visual_settings_labeling_params {
		show: boolean;
		textSize: number;
		percentile: number;
		maxLenPointLabel: number;
		percentile1: number;
	}

	interface Visual_settings_representative_params {
		show: boolean;
		textSize: number;
		maxLenDelegate: number;
	}

	interface Visual_settings_legend_params {
		show: boolean;
		palleteType: string;
		outLierCol: string;
	}

	interface Visual_settings_additional_params {
		showWarnings: boolean;
	}


	/*   interface VisualSettingsVizParams {//appearance 
		 show: boolean;
		 drawEllipse: boolean;
		 drawConvexHull: boolean;
		 drawCentroid: boolean;
		 percentile: number; //TODO: percentage
		 weight: number;   
	  }
  */



    export class Visual implements IVisual {
        private imageDiv: HTMLDivElement;
        private imageElement: HTMLImageElement;

		//        private settings_prepocessing_params: VisualSettingsPreprocessingParams;
		//PBI_TEMPLATE_2: declare groups of parameters
		private settings_prepocessing_params: Visual_settings_prepocessing_params;
		private settings_clusterNum_params: Visual_settings_clusterNum_params;
		private settings_viz_params: Visual_settings_viz_params;
		private settings_labeling_params: Visual_settings_labeling_params;
		private settings_representative_params: Visual_settings_representative_params;
		private settings_legend_params: Visual_settings_legend_params;
		private settings_additional_params: Visual_settings_additional_params;


        public constructor(options: VisualConstructorOptions) {
            this.imageDiv = document.createElement('div');
            this.imageDiv.className = 'rcv_autoScaleImageContainer';
            options.element.appendChild(this.imageDiv);

            this.imageElement = document.createElement('img');
            this.imageElement.className = 'rcv_autoScaleImage';

            this.imageDiv.appendChild(this.imageElement);


            /* this.settings_viz_params = <VisualSettingsVizParams>{
                show: false,
                drawEllipse: false,
                drawConvexHull: false,
                drawCentroid: false,
                percentile: 40,
                weight: 10,
            }; */
			//PBI_TEMPLATE_3: groups of parameters and defaults
			this.settings_prepocessing_params = <Visual_settings_prepocessing_params>{
				show: false,
				scaleData: false,
				applyPCA: false,
			};

			this.settings_clusterNum_params = <Visual_settings_clusterNum_params>{
				granularityParameterType: "auto",
				percentile: 50,
				eps: 0,
				autoParamMinPts: true,
				minPtsParam: 5,
			};

			this.settings_viz_params = <Visual_settings_viz_params>{
				show: true,
				drawEllipse: false,
				drawConvexHull: false,
				drawCenters: false,
				percentile: 30,
				weight: 10,
			};

			this.settings_labeling_params = <Visual_settings_labeling_params>{
				show: true,
				textSize: 8,
				percentile: 100,
				maxLenPointLabel: 5,
				percentile1: 100,
			};

			this.settings_representative_params = <Visual_settings_representative_params>{
				show: false,
				textSize: 8,
				maxLenDelegate: 30,
			};

			this.settings_legend_params = <Visual_settings_legend_params>{
				show: true,
				palleteType: "rainbow",
				outLierCol: "black",
			};

			this.settings_additional_params = <Visual_settings_additional_params>{
				showWarnings: true,
			};




        }

        public update(options: VisualUpdateOptions) {
            let dataViews: DataView[] = options.dataViews;
            if (!dataViews || dataViews.length === 0)
                return;

            let dataView: DataView = dataViews[0];
            if (!dataView || !dataView.metadata)
                return;

			//PBI_TEMPLATE_4: groups of parameters populate values 
			this.settings_prepocessing_params = <Visual_settings_prepocessing_params>{
				show: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'show', false),
				scaleData: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'scaleData', false),
				applyPCA: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'applyPCA', false),
			};

			this.settings_clusterNum_params = <Visual_settings_clusterNum_params>{
				granularityParameterType: getValue<string>(dataView.metadata.objects, 'settings_clusterNum_params', 'granularityParameterType', "auto"),
				percentile: getValue<number>(dataView.metadata.objects, 'settings_clusterNum_params', 'percentile', 50),
				eps: getValue<number>(dataView.metadata.objects, 'settings_clusterNum_params', 'eps', 0),
				autoParamMinPts: getValue<boolean>(dataView.metadata.objects, 'settings_clusterNum_params', 'autoParamMinPts', true),
				minPtsParam: getValue<number>(dataView.metadata.objects, 'settings_clusterNum_params', 'minPtsParam', 5),
			};

			this.settings_viz_params = <Visual_settings_viz_params>{
				show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', true),
				drawEllipse: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawEllipse', false),
				drawConvexHull: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawConvexHull', false),
				drawCenters: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawCenters', false),
				percentile: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'percentile', 30),
				weight: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'weight', 10),
			};

			this.settings_labeling_params = <Visual_settings_labeling_params>{
				show: getValue<boolean>(dataView.metadata.objects, 'settings_labeling_params', 'show', true),
				textSize: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'textSize', 8),
				percentile: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'percentile', 100),
				maxLenPointLabel: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'maxLenPointLabel', 5),
				percentile1: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'percentile1', 100),
			};

			this.settings_representative_params = <Visual_settings_representative_params>{
				show: getValue<boolean>(dataView.metadata.objects, 'settings_representative_params', 'show', false),
				textSize: getValue<number>(dataView.metadata.objects, 'settings_representative_params', 'textSize', 8),
				maxLenDelegate: getValue<number>(dataView.metadata.objects, 'settings_representative_params', 'maxLenDelegate', 30),
			};

			this.settings_legend_params = <Visual_settings_legend_params>{
				show: getValue<boolean>(dataView.metadata.objects, 'settings_legend_params', 'show', true),
				palleteType: getValue<string>(dataView.metadata.objects, 'settings_legend_params', 'palleteType', "rainbow"),
				outLierCol: getValue<string>(dataView.metadata.objects, 'settings_legend_params', 'outLierCol', "black"),
			};

			this.settings_additional_params = <Visual_settings_additional_params>{
				showWarnings: getValue<boolean>(dataView.metadata.objects, 'settings_additional_params', 'showWarnings', true),
			};

			/* this.settings_viz_params = <VisualSettingsVizParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', false),
                drawEllipse: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawEllipse', false),
                drawConvexHull: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawConvexHull', false),
                drawCentroid: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawCentroid', false),
                percentile: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'percentile',40),
                weight: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'weight',10),
            }; */




            let imageUrl: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
            }

            if (imageUrl) {
                this.imageElement.src = imageUrl;
            } else {
                this.imageElement.src = null;
            }

            this.onResizing(options.viewport);
        }

        public onResizing(finalViewport: IViewport): void {
            this.imageDiv.style.height = finalViewport.height + 'px';
            this.imageDiv.style.width = finalViewport.width + 'px';
        }

        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {

				/* case 'settings_labeling_params':
				objectEnumeration.push({
					objectName: objectName,
					properties: {
						show: this.settings_labeling_params.show,                   
						textSize: this.settings_labeling_params.textSize,
						percentile: this.settings_labeling_params.percentile,
						maxLenPointLabel: inMinMax(this.settings_labeling_params.maxLenPointLabel, 1, 100),
						percentile1: inMinMax(this.settings_labeling_params.percentile1, 0, 100)
					 },
					selector: null
				});
				break; */
				//PBI_TEMPLATE_5: populate switch cases
				case 'settings_prepocessing_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							show: this.settings_prepocessing_params.show,
							scaleData: this.settings_prepocessing_params.scaleData,
							applyPCA: this.settings_prepocessing_params.applyPCA,
						},
						selector: null
					});
					break;
				case 'settings_clusterNum_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							granularityParameterType: this.settings_clusterNum_params.granularityParameterType,

						}
					});
					if (this.settings_clusterNum_params.granularityParameterType == "scale") {
						objectEnumeration.push({
							objectName: objectName,
							properties: {
								percentile: inMinMax(this.settings_clusterNum_params.percentile, 1, 100),

							}
						});
					}
					else
						if (this.settings_clusterNum_params.granularityParameterType == "manual") {
							objectEnumeration.push({
								objectName: objectName,
								properties: {
									eps: inMinMax(this.settings_clusterNum_params.eps, 0, 1e+08)
								}
							});
						}
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							autoParamMinPts: this.settings_clusterNum_params.autoParamMinPts
						},
					});
					if (!this.settings_clusterNum_params.autoParamMinPts) {
						objectEnumeration.push({
							objectName: objectName,
							properties: {
								minPtsParam: inMinMax(this.settings_clusterNum_params.minPtsParam, 1, 1000)
							},
							selector: null
						});
					}
					break;
				case 'settings_viz_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							show: this.settings_viz_params.show,
							drawEllipse: this.settings_viz_params.drawEllipse,
							drawConvexHull: this.settings_viz_params.drawConvexHull,
							drawCenters: this.settings_viz_params.drawCenters,
							percentile: inMinMax(this.settings_viz_params.percentile, 0, 100),
							weight: inMinMax(this.settings_viz_params.weight, 1, 50),
						},
						selector: null
					});
					break;
				case 'settings_labeling_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							show: this.settings_labeling_params.show,
							textSize: inMinMax(this.settings_labeling_params.textSize, 8, 40),
							percentile: inMinMax(this.settings_labeling_params.percentile, 0, 100),
							maxLenPointLabel: inMinMax(this.settings_labeling_params.maxLenPointLabel, 1, 100),
							percentile1: inMinMax(this.settings_labeling_params.percentile1, 0, 100),
						},
						selector: null
					});
					break;
				case 'settings_representative_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							show: this.settings_representative_params.show,
							textSize: inMinMax(this.settings_representative_params.textSize, 1, 40),
							maxLenDelegate: inMinMax(this.settings_representative_params.maxLenDelegate, 1, 100),
						},
						selector: null
					});
					break;
				case 'settings_legend_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							show: this.settings_legend_params.show,
							palleteType: this.settings_legend_params.palleteType,
							outLierCol: this.settings_legend_params.outLierCol,
						},
						selector: null
					});
					break;
				case 'settings_additional_params':
					objectEnumeration.push({
						objectName: objectName,
						properties: {
							showWarnings: this.settings_additional_params.showWarnings,
						},
						selector: null
					});
					break;
            };

            return objectEnumeration;
        }
    }
}
