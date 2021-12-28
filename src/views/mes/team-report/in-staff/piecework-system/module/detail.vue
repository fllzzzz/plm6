<template>
  <common-drawer
    ref="drawerRef"
    :title="`${info.processName}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="60%"
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium">
        <span>班组：</span>
        <span>{{ info.leaderName }}</span>
      </el-tag>
      <el-tag type="success" effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="'{y}-{m}-{d}'">{{ query.startDate }}</span> ~ <span v-parse-time="'{y}-{m}-{d}'">{{ query.endDate }}</span>
      </el-tag>
    </template>
    <template #titleRight> </template>
    <template #content>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight"
        row-key="id"
        show-summary
        :summary-method="getSummaries"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer />
        <product-name-columns :productType="query.productType" />
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="showUnit" prop="showUnit" :show-overflow-tooltip="true" label="核算单位" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.showUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column key="mate" prop="mate" :show-overflow-tooltip="true" label="核算量">
          <template v-slot="scope">
            <span>{{ scope.row.mate }}</span>
          </template>
        </el-table-column>
        <el-table-column key="wages" prop="wages" :show-overflow-tooltip="true" label="工序单价(元)">
          <template v-slot="scope">
            <span v-to-fixed="'YUAN'">{{ scope.row.wages }}</span>
          </template>
        </el-table-column>
        <el-table-column key="price" prop="price" :show-overflow-tooltip="true" label="工资(元)">
          <template v-slot="scope">
            <span v-to-fixed="'YUAN'">{{ scope.row.price }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/in-staff/piecework-system'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productNameColumns from '@comp-mes/table-columns/product-name-columns'

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

const tableLoading = ref(false)
const list = ref([])
const query = inject('query')

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const _query = Object.assign(
      {
        factoryId: props.info?.factory?.id,
        productionLineId: props.info?.productionLine?.id,
        workshopId: props.info?.workshop?.id,
        teamId: props.info.teamId
      },
      query
    )
    const { content } = await detail(_query)
    _list = content.map((v) => {
      v.showUnit = useWageQuotaUnit({ wageQuotaType: v.wageQuotaType }).meteUnit
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
    if (column.property === 'price' || column.property === 'quantity') {
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
      }
    }
  })
  return sums
}
</script>
