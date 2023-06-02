<template>
  <el-card v-loading="tableLoading" class="amortization-detail">
    <div>{{ props.detailRow.name }}</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.amount || 0" /> 元</span>
    </div>
    <div>
      综合成本占比 <span class="blue">{{ props.detailRow.rate || 0 }} %</span>
    </div>
    <div class="print-wrap">
      <print-table
        v-permission="permission.printDetail"
        api-key="materialCostRecord"
        :params="{ ...params, name: props.detailRow.name }"
        size="mini"
        type="warning"
      />
    </div>
  </el-card>
  <common-table ref="tableRef" v-loading="tableLoading" :data-format="columnsDataFormat" :data="list" :max-height="maxHeight - 130">
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column prop="classifyName" key="classifyName" label="物料种类" align="left">
      <template #default="{ row }">
        <!-- 是否甲供材料 -->
        <table-cell-tag :show="!!row.boolPartyA" :offset="15" name="甲供" type="partyA" />
        <span>{{ row.classifyName }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="props.detailRow.expenseClassEnum === expenseClassEnum.MAIN_MATERIAL.V"
      prop="thickness"
      key="thickness"
      label="厚度"
      align="center"
    />
    <el-table-column prop="specMerge" key="specMerge" label="规格" align="center" min-width="140" />
    <el-table-column prop="accountingUnit" key="accountingUnit" label="核算单位" align="center" />
    <el-table-column prop="mete" key="mete" label="核算量" align="center" />
    <el-table-column prop="unitPrice" key="unitPrice" label="不含税单价" align="center" />
    <el-table-column prop="amountExcludingVat" key="amountExcludingVat" label="不含税总价" align="center" />
    <el-table-column prop="outboundTime" key="outboundTime" label="出库日期" align="center" width="100" />
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

<script setup>
import { getOutDetail } from '@/api/contract/fortune-report/fortune-report'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

import { expenseClassEnum } from '@enum-ms/contract'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import { toFixed } from '@/utils/data-type'
import { DP } from '@/settings/config'

import usePagination from '@compos/use-pagination'

const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  },
  maxHeight: {
    type: Number,
    default: 400
  }
})

const permission = inject('permission')
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const params = computed(() => {
  const data = {
    projectId: props.detailRow?.projectId
  }
  if (props.detailRow.expenseClassEnum === expenseClassEnum.MAIN_MATERIAL.V) {
    data.basicClassEnum = materialPurchaseClsEnum.STEEL.V
  }
  if (props.detailRow.expenseClassEnum === expenseClassEnum.AUXILIARY_MATERIAL.V) {
    data.basicClassEnum = materialPurchaseClsEnum.MATERIAL.V
  }
  if (props.detailRow.expenseClassEnum === expenseClassEnum.OTHER_MATERIAL.V) {
    data.basicClassEnum = materialPurchaseClsEnum.OTHER.V
  }
  return data
})

watch(
  () => props.detailRow,
  (detail) => {
    if (detail.projectId) {
      nextTick(() => {
        fetchList()
      })
    }
  },
  { deep: true, immediate: true }
)

const tableLoading = ref(false)
const list = ref([])
const totalAmount = ref(0)
const columnsDataFormat = ref([
  ['unitPrice', 'to-thousand'],
  ['amountExcludingVat', 'to-thousand'],
  ['outboundTime', ['parse-time', '{y}-{m}-{d}']]
])

async function fetchList() {
  let _list = []
  totalAmount.value = 0
  try {
    tableLoading.value = true
    const { content = [], totalElements } = await getOutDetail({
      ...params.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    await setSpecInfoToList(content)
    await numFmtByBasicClass(content)
    _list = content.map((row) => {
      if (row.amountExcludingVat) {
        row.unitPrice = toFixed(row.amountExcludingVat / row.mete, DP.YUAN)
      } else {
        row.unitPrice = undefined
        row.amountExcludingVat = undefined
      }
      return row
    })
  } catch (error) {
    console.log('获取材料详情失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.amortization-detail {
  margin-bottom: 20px;
  ::v-deep(.el-card__body) {
    line-height: 29px;
    position: relative;
    padding-right: 334px;
    > div:first-child {
      color: #706f6f;
      font-weight: bold;
      padding-right: 20px;
      display: inline-block;
      vertical-align: middle;
    }
    > div:not(:first-child, .print-wrap) {
      padding: 0 20px;
      display: inline-block;
      vertical-align: middle;
      border-left: 1px solid #ebeef5;
      .blue {
        color: #0079ff;
        font-weight: bold;
      }
    }
    .print-wrap {
      position: absolute;
      height: 29px;
      top: 0;
      right: 20px;
      bottom: 0;
      margin: auto;
    }
  }
}
</style>
