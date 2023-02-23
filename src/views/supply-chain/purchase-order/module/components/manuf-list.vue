<template>
  <div>
    <div class="head-container">
      <monomer-select-area-select
        v-model:monomerId="query.monomerId"
        v-model:areaId="query.areaId"
        :areaType="manufactureTypeEnum.OUTSOURCE.V"
        clearable
        areaClearable
        :project-id="props.projectId"
      />
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="输入编号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="fetchClassList"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="fetchClassList">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
      <div>
        <tag-tabs
          v-if="artifactTypeList.length"
          v-model="query.structureClassId"
          class="filter-item"
          :style="'width:calc(100% - 230px)'"
          style="display: inline-block"
          :data="artifactTypeList"
          itemKey="id"
          @change="fetchList"
        >
          <template #default="{ item }">
            <span>{{ item.name }}(件/kg)：</span>
            <span>{{ item.quantity }}/{{ item.mete }}</span>
          </template>
        </tag-tabs>
        <span style="float: right">
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-plus" @click="handleAdd"> 选择加入 </common-button>
        </span>
      </div>
    </div>
    <common-table
      v-loading="tableLoading"
      :data="tableData"
      :max-height="maxHeight"
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
  </div>
</template>

<script setup>
import { manufClassListGet, manufListGet } from '@/api/supply-chain/requisitions-manage/requisitions'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { manufClsEnum } from '@enum-ms/classification'
import { deepClone, isNotBlank } from '@/utils/data-type'

import usePagination from '@compos/use-pagination'
import tagTabs from '@comp-common/tag-tabs'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['add'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: Number
  },
  maxHeight: {
    type: Number
  }
})

const query = ref({})
const artifactTypeList = ref([])
const selectList = ref([])
const tableLoading = ref(false)
const tableData = ref([])
const form = inject('crud')?.form

// const totalBadge = computed(() => Object.keys(form.manufMergeObj).length)

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchClassList })

watch(
  [() => props.projectId, () => query.value.monomerId, () => query.value.areaId],
  () => {
    query.value.projectId = props.projectId
    fetchClassList()
  },
  { immediate: true }
)

watch(
  () => props.visible,
  (val) => {
    if (val) {
      fetchList()
    }
  },
  { immediate: true }
)

function selectable(row) {
  return !!row.canPurchaseQuantity
}

function resetQuery() {
  query.value.monomerId = undefined
  query.value.areaId = undefined
  query.value.serialNumber = undefined
  fetchClassList()
}

async function fetchClassList() {
  artifactTypeList.value = []
  tableData.value = []
  query.value.structureClassId = undefined
  if (!query.value.projectId) return
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
  if (!query.value.structureClassId) return
  try {
    tableLoading.value = true
    const { content, totalElements } = await manufListGet({ ...query.value, ...queryPage })
    tableData.value = content.map((v) => {
      if (isNotBlank(form.manufListObj?.[v.id])) {
        const _pv = deepClone(form.manufListObj[v.id])
        v.canPurchaseQuantity = v.quantity - v.purchaseQuantity - _pv.curPurchaseQuantity
      } else {
        v.canPurchaseQuantity = v.quantity - v.purchaseQuantity
      }
      v.canPurchaseQuantity = v.canPurchaseQuantity > 0 ? v.canPurchaseQuantity : 0
      v.curPurchaseQuantity = v.canPurchaseQuantity
      // v.measureUnit = '件' // 计量单位
      // v.accountingUnit = '千克' // 核算单位
      v.basicClass = manufClsEnum.STRUC_MANUFACTURED.V
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

function handleAdd() {
  if (!selectList.value.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  emit('add', selectList.value)
}
</script>

<style lang="scss" scoped></style>
