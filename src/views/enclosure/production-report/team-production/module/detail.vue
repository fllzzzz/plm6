<template>
  <common-drawer ref="drawerRef" title="班组产量详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <el-tag size="medium" effect="plain">
        {{ props.info.project }}
      </el-tag>
      <el-tag size="medium" effect="plain">
        {{ props.info.workshop?.name }}
      </el-tag>
      <el-tag size="medium" effect="plain">
        {{ props.info.line?.name }}
      </el-tag>
      <el-tag size="medium" effect="plain">
        {{ props.info.LeaderName }}
      </el-tag>
    </template>
    <template #titleRight>
      <print-table
        v-if="visible"
        v-permission="props.permission.printDetail"
        :params="params"
        api-key="enclosureTeamProductionDetail"
        size="mini"
        type="warning"
      />
    </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column show-overflow-tooltip prop="plan.name" key="plan.name" label="批次" align="center" />
        <el-table-column show-overflow-tooltip prop="name" key="name" label="名称" align="center" />
        <el-table-column show-overflow-tooltip prop="serialNumber" key="serialNumber" label="编号" align="center" />
        <el-table-column show-overflow-tooltip prop="plate" key="plate" label="板型" align="center" />
        <el-table-column show-overflow-tooltip prop="brand" key="brand" label="品牌" align="center" />
        <el-table-column show-overflow-tooltip prop="color" key="color" label="颜色" align="center" />
        <el-table-column show-overflow-tooltip prop="reportQuantity" key="reportQuantity" label="数量" align="center" />
        <el-table-column show-overflow-tooltip prop="reportTotalLength" key="reportTotalLength" label="产量(m)" align="center" />
        <el-table-column show-overflow-tooltip prop="price" key="price" label="单价" align="center" />
        <el-table-column show-overflow-tooltip prop="totalAmount" key="totalAmount" label="总价" align="center" />
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/enclosure/production-report/team-production'
import { defineProps, defineEmits, ref, watch, computed } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const tableLoading = ref(false)
const list = ref([])
const drawerRef = ref()

const dataFormat = ref([
  ['reportTotalLength', ['to-fixed', DP.MES_ENCLOSURE_L__M]],
  ['price', ['to-fixed-ck', 'YUAN']],
  ['totalAmount', ['to-fixed-ck', 'YUAN']]
])

const params = computed(() => {
  return {
    endTime: props.info.endTime,
    startTime: props.info.startTime,
    projectId: props.info.projectId,
    lineId: props.info.line?.id,
    teamId: props.info.teamId,
    price: props.info.price
  }
})

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [
      ['totalAmount', decimalPrecision.enclosure],
      ['reportTotalLength', DP.MES_ENCLOSURE_L__M]
    ]
  })
}

// 获取列表
async function fetchList() {
  try {
    tableLoading.value = true
    const { content, totalElements } = await detail({
      ...params.value,
      ...queryPage
    })
    list.value = content.map((row) => {
      row.reportTotalLength = (row.reportTotalLength || 0) / 1000
      row.totalAmount = (row.price || 0) * row.reportTotalLength
      return row
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取班组产量详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
