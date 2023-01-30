<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div>
        <el-date-picker
          v-model="year"
          type="year"
          size="small"
          style="width: 100px"
          placeholder="选择年"
          class="filter-item"
          value-format="YYYY"
          format="YYYY"
          :clearable="false"
          :disabled-date="disabledDate"
          @change="fetchAuxiliary"
        />
      </div>
      <!-- <div>
        <export-button class="filter-item" v-permission="permission.download"> 辅材费清单 </export-button>
      </div> -->
    </div>
    <common-table
      ref="tableRef"
      :data="list"
      :empty-text="checkPermission(permission.get) ? '暂无数据' : '暂无权限'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column label="月份" prop="month" align="center" width="60px">
        <template #default="{ row }">
          <span>{{ row.month }}</span>
        </template>
      </el-table-column>
      <el-table-column label="完成量（吨）" prop="productionMete" align="center" width="100px">
        <template #default="{ row }">
          <span>{{ row.productionMete !== null ? convertUnits(row.productionMete, 'kg', 't', DP.COM_WT__T) : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column label="气体" prop="gas" align="center">
        <template v-for="item in gasList" :key="item">
          <el-table-column :label="item.name" align="center">
            <el-table-column label="重量" :prop="'mete_' + item.id" :key="'mete_' + item.id" align="center" />
            <el-table-column label="单位" :prop="'accountingUnit_' + item.id" align="center"> </el-table-column>
            <el-table-column label="金额" :prop="'amountExcludingVAT_' + item.id" :key="'amountExcludingVAT_' + item.id" align="center" />
          </el-table-column>
        </template>
        <el-table-column label="小计" prop="gasSubtotal" align="center">
          <el-table-column label="重量" prop="gasWeight" align="center" />
          <el-table-column label="金额" prop="gasAmount" align="center">
            <template #default="{ row }">
              <span>{{ row.gasAmount }}</span>
            </template>
          </el-table-column>
        </el-table-column>
      </el-table-column>
      <el-table-column label="辅材" prop="auxiliaryMaterials" align="center">
        <template v-for="aux in auxiliaryList" :key="aux">
          <el-table-column :label="aux.name" :prop="'amountExcludingVAT_' + aux.id" align="center" />
        </template>
        <el-table-column label="小计" prop="auxSubtotal" align="center">
          <template #default="{ row }">
            <span>{{ row.auxSubtotal }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column label="合计" prop="totalAmount" align="center" width="140px" />
      <el-table-column label="平均单价（元/吨）" prop="aveUnitPrice" align="center" width="140px" />
    </common-table>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { getAuxAnalysis } from '@/api/operation/auxiliary-fee'

import checkPermission from '@/utils/system/check-permission'
import { auxiliaryFeeAnalysisPM as permission } from '@/page-permission/operation'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { parseTime } from '@/utils/date'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

// import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(parseTime(new Date(), '{y}'))
const list = ref([])
const auxiliaryList = ref([])
const gasList = ref([])
const summaryKeyArr = ref([])
const gasWeightArr = ref([])

onMounted(() => {
  fetchAuxiliary()
})

async function fetchAuxiliary() {
  list.value = []
  summaryKeyArr.value = ['gasAmount', 'gasWeight', 'auxSubtotal', 'totalAmount']
  // gasWeightArr.value = ['mete']
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    const data = await getAuxAnalysis({
      year: year.value
    })
    auxiliaryList.value = data?.auxiliary || []
    gasList.value = data?.gas || []
    for (const key in data) {
      if (key !== 'auxiliary' && key !== 'gas') {
        list.value.push({ month: key, ...data[key] })
      }
    }
    list.value.forEach(async (v) => {
      v.productWeightList = []
      await numFmtByBasicClass(v.gas, {
        toNum: true
      })
      await numFmtByBasicClass(v.auxiliary, {
        toNum: true
      })
      v.gas.map((k) => {
        v['mete_' + k.classifyId] = k.mete
        v['amountExcludingVAT_' + k.classifyId] = k.amountExcludingVAT
        v['accountingUnit_' + k.classifyId] = k.accountingUnit
        summaryKeyArr.value.push('mete_' + k.classifyId)
        summaryKeyArr.value.push('amountExcludingVAT_' + k.classifyId)
        gasWeightArr.value.push('mete_' + k.classifyId)
      })
      v.auxiliary.map((k) => {
        v['amountExcludingVAT_' + k.classifyId] = k.amountExcludingVAT
        v['accountingUnit_' + k.classifyId] = k.accountingUnit
        summaryKeyArr.value.push('amountExcludingVAT_' + k.classifyId)
      })
      v.gasWeight = v.gas?.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur?.mete)
        } else {
          return pre
        }
      }, 0)
      v.gasAmount = v.gas?.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur?.amountExcludingVAT)
        } else {
          return pre
        }
      }, 0)
      v.auxSubtotal = v.auxiliary?.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur?.amountExcludingVAT)
        } else {
          return pre
        }
      }, 0)
      v.totalAmount = v.gasAmount + v.auxSubtotal
      v.aveUnitPrice = v.productionMete ? (v.totalAmount / convertUnits(v.productionMete, 'kg', 't')).toFixed(2) : v.totalAmount
    })
    console.log('list.value', list.value)
    list.value = await numFmtByBasicClass(
      list.value,
      {
        toSmallest: false,
        toNum: true
      }
      // {
      //   mete: gasWeightArr.value
      // }
    )
  } catch (error) {
    console.log('获取已出库辅材和气体科目ID失败', error)
  }
}

function disabledDate(time) {
  return time > new Date()
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

//  合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'productionMete') {
      const productWeightList = data.map((item) => convertUnits(item.productionMete, 'kg', 't', DP.COM_WT__T))
      if (!productWeightList.every((value) => isNaN(value))) {
        sums[index] = productWeightList.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index] ? sums[index] : 0
      }
    }
    if (summaryKeyArr.value.indexOf(column.property) > -1) {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index] ? sums[index].toFixed(2) : 0
      }
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
</style>
