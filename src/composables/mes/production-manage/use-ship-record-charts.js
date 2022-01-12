import * as echarts from 'echarts'
import { onMounted, ref } from 'vue'
import { getShipList } from '@/api/mes/production-manage/dashboard/project-dashboard'
import checkPermission from '@/utils/system/check-permission'

export default function useShipRecordEcharts({ elementId, title, globalProjectId, monomerId, month, permission }) {
  let myChart = null
  const echartsLoading = ref(false)
  const list = ref([])

  async function fetchList() {
    if (!checkPermission(permission.shipGet)) {
      return
    }
    if (!monomerId) {
      return
    }
    try {
      const content = await getShipList({
        projectId: globalProjectId.value,
        monomerId: monomerId.value,
        month: month.value
      })
      list.value = content
    } catch (error) {
      console.log('发运记录信息', error)
    }
  }

  onMounted(async () => {
    initChart(elementId, [])
    await updateChart()
  })

  async function updateChart() {
    try {
      echartsLoading.value = true
      await fetchList()
    } catch (error) {
      console.log('发运记录信息', error)
    }
    var option = myChart.getOption()
    const yData = list.value.map(v => v.quantity)
    const xData = list.value.map(v => v.date)
    const total = list.value.reduce((res, cur) => {
      res += cur.quantity || 0
      return res
    }, 0)
    option.series[0].data = yData
    option.xAxis[0].data = xData
    option.title[0].text = total ? '累计发运车次：' + total : '累计发运车次'
    myChart.setOption(option)
    echartsLoading.value = false
  }

  function initChart(elementId) {
    var chartDom = document.getElementById(elementId)
    myChart = echarts.init(chartDom)
    var option

    option = {
      title: {
        text: '累计发运车次',
        left: 'center'
      },
      grid: {
        left: '4.5%',
        right: '1%',
        bottom: '8%'
      },
      tooltip: {
        trigger: 'axis'
      },
      toolbox: {
        show: true,
        feature: {
          magicType: {
            show: true,
            type: ['line', 'bar']
          },
          saveAsImage: {
            show: true
          }
        }
      },
      color: ['#34bfa3'],
      xAxis: {
        type: 'category',
        data: []
      },
      yAxis: {
        type: 'value'
      },
      series: [{
        data: [],
        type: 'line'
      }]
    }

    option && myChart.setOption(option)
  }

  return {
    updateChart,
    echartsLoading
  }
}
