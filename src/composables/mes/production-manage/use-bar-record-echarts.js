import * as echarts from 'echarts'
import { onMounted, ref } from 'vue'
import { productionRecord, outboundRecord } from '@/api/mes/production-manage/dashboard/main-material-track'

export default function useBarRecordEcharts({ elementId, globalProjectId, year, showPRDetail, showORDetail }) {
  let myChart = null
  const echartsLoading = ref(false)
  const prList = ref([])
  const orList = ref([])
  const xData = ref([])
  for (let i = 1; i <= 12; i++) {
    xData.value.push(i + '月')
  }

  async function fetchProductionRecord() {
    try {
      const { content } = await productionRecord({
        projectId: globalProjectId.value,
        year: year.value
      })

      prList.value = formatYData(content)
    } catch (error) {
      console.log('生产记录信息', error)
    }
  }

  async function fetchOutboundRecord() {
    try {
      const {
        content
      } = await outboundRecord({
        projectId: globalProjectId.value,
        year: year.value
      })
      orList.value = formatYData(content)
    } catch (error) {
      console.log('出库记录信息', error)
    }
  }

  function formatYData(list) {
    const arr = new Array(12)
    for (let i = 0; i < 12; i++) {
      arr[i] = { name: (i + 1) + '月', groupId: i + 1, value: 0 }
    }
    for (let i = 0; i < list.length; i++) {
      const index = Number(list[i].groupKey) - 1
      arr[index].value = list[i].mete.toFixed(2)
    }
    return arr
  }

  onMounted(async () => {
    initChart(elementId, xData.value)
    await updateChart()
  })

  async function updateChart() {
    try {
      echartsLoading.value = true
      await fetchProductionRecord()
      await fetchOutboundRecord()
    } catch (error) {
      console.log('生产、出库记录信息', error)
    }
    var option = myChart.getOption()
    option.series[0].data = orList.value
    option.series[1].data = prList.value
    myChart.setOption(option)
    echartsLoading.value = false
  }

  function initChart(elementId, xData) {
    var app = {}

    var chartDom = document.getElementById(elementId)
    myChart = echarts.init(chartDom, null, { locale: 'ZH' })
    var option

    const posList = [
      'left',
      'right',
      'top',
      'bottom',
      'inside',
      'insideTop',
      'insideLeft',
      'insideRight',
      'insideBottom',
      'insideTopLeft',
      'insideTopRight',
      'insideBottomLeft',
      'insideBottomRight'
    ]
    app.configParameters = {
      rotate: {
        min: -90,
        max: 90
      },
      align: {
        options: {
          left: 'left',
          center: 'center',
          right: 'right'
        }
      },
      verticalAlign: {
        options: {
          top: 'top',
          middle: 'middle',
          bottom: 'bottom'
        }
      },
      position: {
        options: posList.reduce(function (map, pos) {
          map[pos] = pos
          return map
        }, {})
      },
      distance: {
        min: 0,
        max: 100
      }
    }
    app.config = {
      rotate: 90,
      align: 'center',
      verticalAlign: 'middle',
      position: 'insideBottom',
      distance: 15,
      onChange: function () {
        const labelOption = {
          rotate: app.config.rotate,
          align: app.config.align,
          verticalAlign: app.config.verticalAlign,
          position: app.config.position,
          distance: app.config.distance
        }
        myChart.setOption({
          series: [{
            label: labelOption
          },
          {
            label: labelOption
          },
          {
            label: labelOption
          },
          {
            label: labelOption
          }
          ]
        })
      }
    }
    const labelOption = {
      show: false,
      position: app.config.position,
      distance: app.config.distance,
      align: app.config.align,
      verticalAlign: app.config.verticalAlign,
      rotate: app.config.rotate,
      formatter: '{c}',
      color: '#fff',
      fontSize: 14,
      rich: {
        name: {}
      }
    }
    option = {
      tooltip: {
        show: true,
        trigger: 'axis',
        axisPointer: {
          type: 'shadow'
        }
      },
      grid: {
        left: '4.5%',
        right: '1%',
        bottom: '8%'
      },
      color: ['#fb6e52', '#48cfae'],
      legend: {
        data: ['钢材出库记录', '构件生产记录']
      },
      toolbox: {
        show: true,
        orient: 'horizontal',
        left: 'right',
        top: 'top',
        feature: {
          mark: {
            show: true
          },
          dataView: {
            show: false
          },
          magicType: {
            show: true,
            type: ['line', 'bar']
          },
          restore: {
            show: false
          },
          saveAsImage: {
            show: true
          }
        }
      },
      xAxis: [{
        type: 'category',
        axisTick: {
          show: false
        },
        data: xData
      }],
      yAxis: [{
        type: 'value'
      }],
      series: [{
        name: '钢材出库记录',
        type: 'bar',
        barGap: 0.3,
        label: labelOption,
        emphasis: {
          focus: 'series'
        },
        data: []
      },
      {
        name: '构件生产记录',
        type: 'bar',
        label: labelOption,
        emphasis: {
          focus: 'series'
        },
        data: []
      }
      ]
    }

    option && myChart.setOption(option)
    // 添加点击事件
    myChart.on('click', function (params) {
      if (params.seriesIndex === 0) {
        showORDetail(params.data.groupId)
      }
      if (params.seriesIndex === 1) {
        showPRDetail(params.data.groupId)
      }
    })
  }

  return {
    updateChart,
    echartsLoading
  }
}
