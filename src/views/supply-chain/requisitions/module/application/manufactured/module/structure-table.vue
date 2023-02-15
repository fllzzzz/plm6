<template>
  <div class="requisitions-application-header-class">
    <tag-tabs
      v-if="artifactTypeList.length"
      v-model="query.structureClassId"
      class="filter-item"
      style="width: 100%; display: inline-block; margin-bottom: 10px"
      :data="artifactTypeList"
      itemKey="id"
      @change="fetchList"
    >
      <template #default="{ item }">
        <span>{{ item.name }}(件/kg)：</span>
        <span>{{ item.quantity }}/{{ item.mete }}</span>
      </template>
    </tag-tabs>
  </div>
  <common-table
    v-bind="$attrs"
    v-loading="tableLoading"
    :data="tableData"
    style="width: 100%"
    @selection-change="handleSelectionChange"
  >
    <el-table-column type="selection" width="55" align="center" :selectable="selectable" />
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
    <el-table-column prop="length" label="长度（mm）" align="center" show-overflow-tooltip />
    <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="quantity" label="清单数量" align="center" show-overflow-tooltip />
    <el-table-column prop="canPurchaseQuantity" label="可采购数量" align="center" show-overflow-tooltip />
    <el-table-column label="采购数量" align="center" show-overflow-tooltip min-width="100px">
      <template #default="{ row: { sourceRow: row } }">
        <common-input-number
          v-model="row.curPurchaseQuantity"
          :min="0"
          :max="row.canPurchaseQuantity"
          controls-position="right"
          :controls="false"
          :step="5"
          size="mini"
          placeholder="数量"
        />
      </template>
    </el-table-column>
    <el-table-column prop="totalNetWeight" label="总重（kg）" align="center" show-overflow-tooltip />
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
import { manufClassListGet, manufListGet } from '@/api/supply-chain/requisitions-manage/requisitions'
import { ref, defineExpose, inject } from 'vue'
import { deepClone, isNotBlank, isBlank } from '@/utils/data-type'

import { regExtra } from '@/composables/form/use-form'
import usePagination from '@compos/use-pagination'
import tagTabs from '@comp-common/tag-tabs'

const artifactTypeList = ref([])
const selectList = ref([])
const tableLoading = ref(false)
const tableData = ref([])
const { form } = regExtra() // 表单
const query = inject('customQuery')

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchClassList })

function selectable(row) {
  return !!row.canPurchaseQuantity
}

async function fetchClassList() {
  artifactTypeList.value = []
  tableData.value = []
  query.value.structureClassId = undefined
  if (isBlank(query.value.projectId)) return
  try {
    const { content } = await manufClassListGet({ ...query.value })
    if (content?.length) {
      artifactTypeList.value = content
      query.value.structureClassId = content[0]?.id
      fetchList()
    }
  } catch (er) {
    console.log(er, '获取构件类型')
  }
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { content, totalElements } = await manufListGet({ ...query.value, ...queryPage })
    tableData.value = content.map((v) => {
      if (isNotBlank(form.purchaseListObj[v.id])) {
        const _pv = deepClone(form.purchaseListObj[v.id])
        v.canPurchaseQuantity = v.quantity - v.purchaseQuantity - _pv.curPurchaseQuantity
      } else {
        v.canPurchaseQuantity = v.quantity - v.purchaseQuantity
      }
      v.canPurchaseQuantity = v.canPurchaseQuantity > 0 ? v.canPurchaseQuantity : 0
      v.curPurchaseQuantity = v.canPurchaseQuantity
      v.measureUnit = '件' // 计量单位
      v.accountingUnit = '千克' // 核算单位
      return v
    })
    setTotalPage(totalElements)
  } catch (er) {
    console.log(er, '获取成品清单')
  } finally {
    tableLoading.value = false
  }
}

function handleSelectionChange(val) {
  selectList.value = val
}

defineExpose({
  refresh: fetchClassList,
  refreshList: fetchList,
  selectList: selectList
})
</script>

<style lang="scss" scoped></style>
