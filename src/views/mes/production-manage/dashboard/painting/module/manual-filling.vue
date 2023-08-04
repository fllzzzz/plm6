<template>
  <common-drawer ref="drawerRef" title="手工填报" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="80%">
    <template #content>
      <div class="head-container manual-filling-head">
        <monomer-select-area-select
          v-model:monomerId="query.monomerId"
          v-model:areaId="query.areaId"
          clearable
          areaClearable
          :project-id="props.projectId"
          @change="fetch"
        />
        <div>
          <export-button
            v-if="checkPermission(permission.download)"
            :fn="downloadManualList"
            :params="params"
            :disabled="!tableData.length"
          >
            下载油漆填报明细(按条件查询)
          </export-button>
          <div v-permission="permission.edit" style="float: right">
            <template v-if="isEdit">
              <common-button size="mini" @click.stop="closeEdit">取消</common-button>
              <common-button size="mini" type="success" @click="previewIt">预览并保存</common-button>
            </template>
            <common-button v-else type="primary" size="mini" @click="openEdit">编辑</common-button>
          </div>
        </div>
      </div>
      <common-table
        v-loading="tableLoading"
        return-source-data
        :data="tableData"
        :show-empty-symbol="false"
        :cell-class-name="changedCellMask"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" align="center" min-width="120px" />
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" align="center" min-width="120px" />
        <el-table-column prop="artifactClass.name" label="构件类型" align="center" min-width="100px" />
        <el-table-column prop="paintArea" label="油漆面积(㎡)" align="center" min-width="100px">
          <template #default="{ row }">
            <div v-if="!isEdit">{{ row.sourcePaintArea }}</div>
            <el-input-number
              v-else
              v-model="row.paintArea"
              :step="100"
              :min="0"
              :precision="DP.COM_AREA__M2"
              size="mini"
              style="width: 100%"
              controls-position="right"
              placeholder="油漆面积"
            />
          </template>
        </el-table-column>

        <el-table-column label="底漆" align="center">
          <el-table-column prop="primerBrand" label="品牌" align="center" min-width="100px">
            <template #default="{ row }">
              <div v-if="!isEdit">{{ row.sourcePrimerBrand }}</div>
              <el-input v-else v-model="row.primerBrand" maxlength="64" size="mini" style="width: 100%" placeholder="品牌" />
            </template>
          </el-table-column>
          <el-table-column prop="primerDosage" label="用量(kg)" align="center" min-width="100px">
            <template #default="{ row }">
              <div v-if="!isEdit">{{ row.sourcePrimerDosage }}</div>
              <el-input-number
                v-else
                v-model="row.primerDosage"
                :step="100"
                :min="0"
                :precision="DP.COM_WT__KG"
                size="mini"
                style="width: 100%"
                controls-position="right"
                placeholder="用量"
              />
            </template>
          </el-table-column>
        </el-table-column>

        <el-table-column label="中间漆" align="center">
          <el-table-column prop="intermediatePaintBrand" label="品牌" align="center" min-width="100px">
            <template #default="{ row }">
              <div v-if="!isEdit">{{ row.sourceIntermediatePaintBrand }}</div>
              <el-input
                v-else
                v-model="row.intermediatePaintBrand"
                maxlength="64"
                size="mini"
                style="width: 100%"
                placeholder="品牌"
                @change="change"
              />
            </template>
          </el-table-column>
          <el-table-column prop="intermediatePaintDosage" label="用量(kg)" align="center" min-width="100px">
            <template #default="{ row }">
              <div v-if="!isEdit">{{ row.sourceIntermediatePaintDosage }}</div>
              <el-input-number
                v-else
                v-model="row.intermediatePaintDosage"
                :step="100"
                :min="0"
                :precision="DP.COM_WT__KG"
                size="mini"
                style="width: 100%"
                controls-position="right"
                placeholder="用量"
              />
            </template>
          </el-table-column>
        </el-table-column>

        <el-table-column label="面漆" align="center">
          <el-table-column prop="topcoatBrand" label="品牌" align="center" min-width="100px">
            <template #default="{ row }">
              <div v-if="!isEdit">{{ row.sourceTopcoatBrand }}</div>
              <el-input v-else v-model="row.topcoatBrand" maxlength="64" size="mini" style="width: 100%" placeholder="品牌" />
            </template>
          </el-table-column>
          <el-table-column prop="topcoatDosage" label="用量(kg)" align="center" min-width="100px">
            <template #default="{ row }">
              <div v-if="!isEdit">{{ row.sourceTopcoatDosage }}</div>
              <el-input-number
                v-else
                v-model="row.topcoatDosage"
                :step="100"
                :min="0"
                :precision="DP.COM_WT__KG"
                size="mini"
                style="width: 100%"
                controls-position="right"
                placeholder="用量"
              />
            </template>
          </el-table-column>
        </el-table-column>
      </common-table>
      <manual-filling-preview v-model:visible="previewVisible" :data="tableData" @saveSuccess="handleSaveSuccess" />
    </template>
  </common-drawer>
</template>

<script setup>
import { downloadManualList, manualList } from '@/api/mes/production-manage/dashboard/painting'
import { defineProps, defineEmits, computed, provide, ref, watch } from 'vue'

import { DP } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'
import { paintingDashboardPM } from '@/page-permission/mes'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useTableChange from '@compos/form/use-table-change'
import manualFillingPreview from './manual-filling-preview'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import useVisible from '@compos/use-visible'
import ExportButton from '@comp-common/export-button/index.vue'

const emit = defineEmits(['update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const { maxHeight } = useMaxHeight(
  {
    extraBox: '.manual-filling-head'
  },
  drawerVisible
)

const sourceMap = new Map([
  ['paintArea', 'sourcePaintArea'],
  ['primerBrand', 'sourcePrimerBrand'],
  ['primerDosage', 'sourcePrimerDosage'],
  ['topcoatBrand', 'sourceTopcoatBrand'],
  ['topcoatDosage', 'sourceTopcoatDosage'],
  ['intermediatePaintBrand', 'sourceIntermediatePaintBrand'],
  ['intermediatePaintDosage', 'sourceIntermediatePaintDosage']
])
provide('sourceMap', sourceMap)

const permission = paintingDashboardPM.manual
const isEdit = ref(false)
const previewVisible = ref(false)
const query = ref({})
const tableLoading = ref(false)
const tableData = ref([])
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

const params = computed(() => {
  return {
    ...query.value,
    projectId: props.projectId
  }
})

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetch()
    }
  },
  { immediate: true }
)

function change(val) {
  console.log('val: ', val, typeof val)
}
async function fetch() {
  tableData.value = []
  try {
    tableLoading.value = true
    const data = await manualList(params.value)
    tableData.value = data.map((v) => {
      v.areaId = v.area.id
      v.monomerId = v.monomer.id
      v.projectId = props.projectId
      v.structureClassId = v.artifactClass.id
      v.primerBrand = v.primerBrand || ''
      v.topcoatBrand = v.topcoatBrand || ''
      v.intermediatePaintBrand = v.intermediatePaintBrand || ''
      v.sourcePaintArea = v.paintArea
      v.sourcePrimerBrand = v.primerBrand
      v.sourcePrimerDosage = v.primerDosage
      v.sourceTopcoatBrand = v.topcoatBrand
      v.sourceTopcoatDosage = v.topcoatDosage
      v.sourceIntermediatePaintBrand = v.intermediatePaintBrand
      v.sourceIntermediatePaintDosage = v.intermediatePaintDosage
      return v
    })
    console.log('tableData.value : ', tableData.value)
  } catch (er) {
    console.log('获取手工填报数据', er)
  } finally {
    tableLoading.value = false
  }
}

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['paintArea', 'primerDosage', 'topcoatDosage', 'intermediatePaintDosage']
  })
  return summary
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
  tableData.value.forEach((v) => {
    v.paintArea = v.sourcePaintArea
    v.primerBrand = v.sourcePrimerBrand
    v.primerDosage = v.sourcePrimerDosage
    v.topcoatBrand = v.sourceTopcoatBrand
    v.topcoatDosage = v.sourceTopcoatDosage
    v.intermediatePaintBrand = v.sourceIntermediatePaintBrand
    v.intermediatePaintDosage = v.sourceIntermediatePaintDosage
  })
}

function previewIt() {
  previewVisible.value = true
}
</script>

<style lang="scss" scoped>
::v-deep(.el-input-number.is-controls-right .el-input__inner) {
  padding-left: 5px;
  padding-right: 35px;
}
</style>
