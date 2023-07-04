<template>
  <common-drawer ref="drawerRef" title="计价制详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleAfter>
      <el-tag type="success" effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="{ val: query.startDate, fmt: '{y}-{m}-{d}' }" /> ~
        <span v-parse-time="{ val: query.endDate, fmt: '{y}-{m}-{d}' }" />
      </el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          api-key="mesPieceworkDetail"
          :params="printParams"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="list"
        :data-format="dataFormat"
        :max-height="maxHeight"
        row-key="rowId"
        show-summary
        :summary-method="getSummaries"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showFactory showProductionLine showProcess showTeam />
        <productType-base-info-columns :productType="query.productType" :unShowField="['color']" />
        <el-table-column prop="completeQuantity" :show-overflow-tooltip="true" label="生产数量" align="center">
          <template #default="{ row }">
            <span>{{ row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="mete" :show-overflow-tooltip="true" :label="`${unitObj.label}(${unitObj.unit})`" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="showUnit" :show-overflow-tooltip="true" label="核算单位" align="center">
          <template #default="{ row }">
            <span>{{ row.showUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="checkMete" :show-overflow-tooltip="true" label="产量" align="center">
          <template #default="{ row }">
            <span>{{ row.checkMete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="wage" :show-overflow-tooltip="true" label="工序单价(元)" align="center">
          <template #default="{ row }">
            <span>{{ row.wage }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="price" :show-overflow-tooltip="true" label="工资(元)" align="center">
          <template #default="{ row }">
            <span>{{ row.price }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/in-staff/piecework-system'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
// import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const dataFormat = ref([
  ['wage', ['to-fixed-ck', 'YUAN']],
  ['price', ['to-fixed-ck', 'YUAN']]
])

const tableLoading = ref(false)
const list = ref([])
const query = inject('query')
const permission = inject('permission')

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({
    productType: query.productType
  })
})

const printParams = computed(() => {
  return Object.assign(
    {
      factoryId: props.info?.factory?.id,
      productionLineId: props.info?.productionLine?.id,
      workshopId: props.info?.workshop?.id,
      teamId: props.info.teamId,
      processId: props.info.processId
    },
    query
  )
})

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content } = await detail(printParams.value)
    _list = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.showUnit = useWageQuotaUnit({ wageQuotaType: v.wageQuotaType }).meteUnit
      // v.checkMete = useWageQuotaMeteConvert({
      //   length: v.mate,
      //   weight: v.mate,
      //   surfaceArea: v.mate,
      //   wageQuotaType: v.wageQuotaType
      // }).convertMete
      v.checkMete = v.mate
      v.mete = useProductMeteConvert({
        productType: query.productType,
        length: { num: v.length * v.completeQuantity, to: unitObj.value.unit, dp: unitObj.value.dp },
        weight: { num: v.netWeight * v.completeQuantity, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      return v
    })
  } catch (error) {
    console.log('获取详情列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'price' || column.property === 'completeQuantity') {
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
        if (column.property === 'price') {
          sums[index] = sums[index].toFixed(decimalPrecision.value.mes)
        }
      }
    }
  })
  return sums
}
</script>
