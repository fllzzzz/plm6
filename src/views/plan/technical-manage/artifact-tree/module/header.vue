<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :productType="TechnologyTypeAllEnum.STRUCTURE.V"
        @getAreaInfo="getAreaInfo"
      />
      <area-tabs
        class="filter-item"
        style="width: calc(100% - 230px)"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-input
        v-model="query.artifactName"
        size="small"
        placeholder="输入构件名称搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.artifactSerialNumber"
        size="small"
        placeholder="输入构件编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.machinePartSerialNumber"
        size="small"
        placeholder="输入零件编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          v-if="currentArea && currentArea.id"
          v-permission="crud.permission.import"
          :data="{ areaId: crud.query.areaId }"
          :upload-fun="listUpload"
          btn-name="零构件清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="uploadSuccess"
        />
        <export-button
          v-if="currentArea && currentArea.id"
          :fn="downloadArtifactTree"
          :params="carryParam"
          show-btn-text
          btn-text="零构件清单（按构件条件查询）"
          class="filter-item"
        />
        <export-button :fn="downloadArtifactTreeTemplate" show-btn-text btn-text="零构件清单模板" class="filter-item" />
      </template>
      <template #viewLeft>
        <el-tooltip
          effect="light"
          :content="`${mismatchList.join(',')}`"
          placement="top"
        >
          <div class="filter-item">
            <el-tag v-if="mismatchList.length>0" type="danger" class="filter-item" effect="plain">存在{{ mismatchList.length }}条错误数据，鼠标悬停查看</el-tag>
          </div>
        </el-tooltip>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import { listUpload } from '@/api/plan/technical-manage/artifact-tree'
import ExportButton from '@comp-common/export-button/index.vue'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { downloadArtifactTree, downloadArtifactTreeTemplate, errorArtifact } from '@/api/plan/technical-manage/artifact-tree'

const defaultQuery = {
  artifactName: '',
  artifactSerialNumber: '',
  machinePartSerialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const mismatchList = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => query.areaId,
  (val) => {
    if (val) {
      getErrorArtifactData()
    }
  },
  { immediate: true, deep: true }
)

const carryParam = computed(() => {
  const param = { ...crud.query }
  delete param.machinePartSerialNumber
  return param
})

function uploadSuccess() {
  getErrorArtifactData()
  crud.toQuery()
}
function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  crud.toQuery()
}
function getAreaInfo(val) {
  areaInfo.value = val || []
  if (areaInfo.value.length > 0) {
    defaultTab.value = {
      id: areaInfo.value[0].id + '',
      name: areaInfo.value[0].name
    }
  } else {
    defaultTab.value = {}
  }
}

async function getErrorArtifactData() {
  mismatchList.value = []
  try {
    const { content } = await errorArtifact({ areaId: crud.query.areaId })
    if (content && content.length > 0) {
      content.map(v => {
        mismatchList.value.push(v.serialNumber)
      })
    }
  } catch (e) {
    console.log('获取异常构件', e)
  }
}
</script>
