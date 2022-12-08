<template>
  <div class="artifact-list">
    <div class="head-container artifact-list-head">
      <monomer-select-area-select
        v-model:monomerId="query.monomerId"
        v-model:areaId="query.areaId"
        clearable
        areaClearable
        :project-id="projectId"
        :monomerDisabled="!projectId || !!crud.query.monomerId"
        :areaDisabled="!projectId || !!crud.query.areaId"
        @change="fetch"
      />
      <!--工具栏-->
      <div v-permission="permission.editArea" style="float: right">
        <template v-if="isEdit">
          <common-button size="mini" @click.stop="closeEdit">取消</common-button>
          <common-button size="mini" type="success" @click="previewIt">预览并保存</common-button>
        </template>
        <common-button v-else type="primary" size="mini" @click="openEdit">编辑【面积】</common-button>
      </div>
      <br />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="fetch"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="fetch">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery()">
        重置
      </common-button>
      <el-tag size="medium" class="filter-item" style="float: right" type="success" effect="plain">
        总数：{{ summaryInfo.quantity || 0 }}件 | 总面积：{{ summaryInfo.surfaceArea || 0 }}㎡</el-tag
      >
    </div>
    <common-table
      v-loading="tableLoading"
      return-source-data
      :data="tableData"
      :cell-class-name="changedCellMask"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" align="center" min-width="120px" />
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" align="center" min-width="120px" />
      <el-table-column prop="serialNumber" label="编号" align="center" min-width="80px" />
      <el-table-column prop="quantity" label="编号" align="center" min-width="80px" />
      <el-table-column prop="surfaceArea" label="面积(㎡)" align="center" min-width="80px">
        <template #default="{ row }">
          <div v-if="!isEdit">{{ row.sourceSurfaceArea }}</div>
          <el-input-number
            v-else
            v-model="row.surfaceArea"
            :step="1"
            :min="0"
            :precision="DP.COM_AREA__M2"
            size="mini"
            style="width: 100%"
            controls-position="right"
            placeholder="面积"
          />
        </template>
      </el-table-column>
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
    <batch-edit-area-preview v-model:visible="previewVisible" :data="tableData" @saveSuccess="handleSaveSuccess" />
  </div>
</template>

<script setup>
import { artifactList, artifactSummary } from '@/api/bridge/bridge-production-manage/painting'
import { defineProps, provide, ref, watch, inject } from 'vue'

import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useTableChange from '@compos/form/use-table-change'
import batchEditAreaPreview from './batch-edit-area-preview'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  configId: {
    type: Number,
    default: undefined
  }
})

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetch })

const { maxHeight } = useMaxHeight({
  extraBox: '.artifact-list-head',
  paginate: true
})

const sourceMap = new Map([['surfaceArea', 'sourceSurfaceArea']])
provide('sourceMap', sourceMap)

const crud = inject('crud')
const permission = inject('permission')
const isEdit = ref(false)
const previewVisible = ref(false)
const summaryInfo = ref({})
const query = ref({})
const tableLoading = ref(false)
const tableData = ref([])
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

function resetQuery() {
  query.value = { ...crud.query }
  queryPage.pageNumber = 1
  fetch()
}

watch(
  () => props.configId,
  () => {
    resetQuery()
  },
  { immediate: true }
)

async function fetch() {
  tableData.value = []
  summaryInfo.value = {}
  if (!props.configId) return
  try {
    tableLoading.value = true
    summaryInfo.value = (await artifactSummary({ configId: props.configId, ...query.value })) || {}
    summaryInfo.value.surfaceArea = convertUnits(summaryInfo.value.surfaceArea, 'mm²', '㎡', DP.COM_AREA__M2)
    const { content, totalElements } = await artifactList({ configId: props.configId, ...query.value, ...queryPage })
    tableData.value = content.map((v) => {
      const area = convertUnits(v.surfaceArea, 'mm²', '㎡', DP.COM_AREA__M2)
      v.sourceSurfaceArea = area
      v.surfaceArea = area
      return v
    })
    setTotalPage(totalElements)
  } catch (er) {
    console.log('获取构件清单', er)
  } finally {
    tableLoading.value = false
  }
}

function handleSaveSuccess() {
  isEdit.value = false
  fetch()
}

// 编辑
function openEdit() {
  isEdit.value = true
}

// 取消编辑
function closeEdit() {
  isEdit.value = false
}

function previewIt() {
  previewVisible.value = true
}
</script>

<style lang="scss" scoped></style>
