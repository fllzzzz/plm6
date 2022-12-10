<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <div class="head-container">
        <mHeader />
      </div>
      <common-table
        ref="tableRef"
        class="painting-crud-table"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        row-key="id"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('monomer.name')"
          prop="monomer.name"
          align="center"
          :show-overflow-tooltip="true"
          label="单体"
          min-width="120px"
        />
        <el-table-column v-if="columns.visible('surfaceArea')" prop="surfaceArea" label="面积(㎡)" align="center" min-width="80px" />
        <el-table-column v-if="columns.visible('primerPainting')" prop="primerPainting" label="底漆(um)" align="center" min-width="80px">
          <template #default="{ row }">
            <div style="display: flex; align-items: center">
              <span style="flex: 1">{{ row.primerPainting?.thickness || '-' }}</span>
              <el-icon
                v-permission="permission.edit"
                class="pointer"
                @click="toEditForm(row, row.primerPainting, paintingTypeEnum.PRIMER.V)"
              >
                <el-edit />
              </el-icon>
            </div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('intermediatePainting')"
          prop="intermediatePainting"
          label="中间漆(um)"
          align="center"
          min-width="80px"
        >
          <template #default="{ row }">
            <div style="display: flex; align-items: center">
              <span style="flex: 1">{{ row.intermediatePainting?.thickness || '-' }}</span>
              <el-icon
                v-permission="permission.edit"
                class="pointer"
                @click="toEditForm(row, row.intermediatePainting, paintingTypeEnum.INTERMEDIATE_PAINT.V)"
              >
                <el-edit />
              </el-icon>
            </div>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('topcoatPainting')" prop="topcoatPainting" label="面漆(um)" align="center" min-width="80px">
          <template #default="{ row }">
            <div style="display: flex; align-items: center">
              <span style="flex: 1">{{ row.topcoatPainting?.thickness || '-' }}</span>
              <el-icon
                v-permission="permission.edit"
                class="pointer"
                @click="toEditForm(row, row.topcoatPainting, paintingTypeEnum.TOPCOAT.V)"
              >
                <el-edit />
              </el-icon>
            </div>
          </template>
        </el-table-column>
      </common-table>
      <el-divider class="painting-divider"><span class="title">油漆用量明细 </span></el-divider>
      <div class="painting-artifact-head head-container">
        <el-tag effect="plain" type="success" size="medium" class="filter-item">理论涂布率：9.8L/平方米</el-tag>
        <print-table
          v-permission="permission.print"
          api-key="bridgePaintingList"
          :params="{ ...crud.query }"
          size="mini"
          type="warning"
          style="float: right"
          class="filter-item"
        />
      </div>
      <common-table
        ref="artifactTableRef"
        class="painting-artifact-table"
        v-loading="artifactLoading"
        :data="allArtifactList"
        :max-height="artifactMaxHeight"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        row-key="id"
        highlight-current-row
        @current-change="currentChange"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="config.name" :show-overflow-tooltip="true" label="分段类型" align="center" min-width="120px" />
        <el-table-column prop="surfaceArea" label="涂装面积(㎡)" align="center" min-width="80px" />
        <el-table-column prop="primerMeasure" label="底漆(L)" align="center" min-width="80px" />
        <el-table-column prop="intermediateMeasure" label="中间漆(L)" align="center" min-width="80px" />
        <el-table-column prop="topcoatMeasure" label="面漆(L)" align="center" min-width="80px" />
      </common-table>
    </div>
    <el-divider direction="vertical" :style="`height: ${lineMaxHeight}px`"></el-divider>
    <div class="wrap-right">
      <div v-show="!configId" class="my-code">*点击左侧油漆用量明细查看</div>
      <artifact-list v-show="configId" :projectId="crud.query.projectId" :configId="configId" />
    </div>
    <edit-form v-model:visible="editFormVisible" :info="itemInfo" @refresh="crud.toQuery"></edit-form>
  </div>
</template>

<script setup>
import crudApi, { getAllArtifact } from '@/api/bridge/production-manage/dashboard/painting'
import { ref } from 'vue'

import { paintingTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { tableSummary } from '@/utils/el-extra'
import checkPermission from '@/utils/system/check-permission'
import { bridgePaintingDashboardPM as permission } from '@/page-permission/bridge'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import artifactList from './module/artifact-list.vue'
import editForm from './module/edit-form.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const artifactTableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '涂装计算',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.painting-divider', '.painting-artifact-head', '.painting-artifact-table'],
  minHeight: 200,
  paginate: false
})

const { maxHeight: lineMaxHeight } = useMaxHeight({
  extraBox: '',
  paginate: false
})
const { maxHeight: artifactMaxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.painting-divider', '.painting-artifact-head', '.painting-crud-table'],
  minHeight: 200,
  paginate: false
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  fetchAllArtifact()
  res.data.content = res.data.content.map((v, i) => {
    v.id = i + '' + Math.random()
    v.surfaceArea = convertUnits(v.surfaceArea, 'mm²', '㎡', DP.COM_AREA__M2)
    return v
  })
}

const dataFormat = ref([
  ['primerMeasure', ['to-fixed-ck', 'COM_VOLUME__L']],
  ['intermediateMeasure', ['to-fixed-ck', 'COM_VOLUME__L']],
  ['topcoatMeasure', ['to-fixed-ck', 'COM_VOLUME__L']]
])

const editFormVisible = ref(false)
const itemInfo = ref({})
const configId = ref()
const allArtifactList = ref([])
const artifactLoading = ref(false)

function toEditForm(row, paintRow, type) {
  if (!checkPermission(permission.edit)) return
  itemInfo.value = {
    surfaceArea: row.surfaceArea,
    projectId: row.project?.id,
    monomerId: row.monomer?.id,
    thickness: paintRow.thickness,
    loss: paintRow.loss ? paintRow.loss * 100 : paintRow.loss,
    volumeSolids: paintRow.volumeSolids ? paintRow.volumeSolids * 100 : paintRow.volumeSolids,
    paintingType: type
  }
  editFormVisible.value = true
}

async function fetchAllArtifact() {
  try {
    allArtifactList.value = []
    configId.value = undefined
    artifactLoading.value = true
    const { content } = await getAllArtifact({ ...crud.query })
    allArtifactList.value = content.map((v) => {
      v.surfaceArea = convertUnits(v.surfaceArea, 'mm²', '㎡', DP.COM_AREA__M2)
      return v
    })
  } catch (er) {
    console.log('获取全部分段信息失败', er)
  } finally {
    artifactLoading.value = false
  }
}

function currentChange(row) {
  configId.value = row.config?.id
}

function getSummaries(param) {
  return tableSummary(param, { props: ['surfaceArea', 'primerMeasure', 'intermediateMeasure', 'topcoatMeasure'] })
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 50%;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
