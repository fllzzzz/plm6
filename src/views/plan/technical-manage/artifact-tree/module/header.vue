<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :productType="TechnologyTypeAllEnum.STRUCTURE.V"
        :show-tips="areaInfo.length <= 0"
        @getAreaInfo="getAreaInfo"
        @change="monomerChange"
      />
      <area-tabs
        class="filter-item"
        :style="areaInfo.length > 0 ? 'width:calc(100% - 230px)' : 'width:calc(100% - 380px)'"
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
      />
      <el-input
        v-model="query.artifactSerialNumber"
        size="small"
        placeholder="输入构件编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.machinePartSerialNumber"
        size="small"
        placeholder="输入零件编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <!-- <upload-btn
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.import)"
          :data="{ areaId: crud.query.areaId, importType: 1 }"
          :upload-fun="listUpload"
          btn-name="清单增量导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="uploadSuccess"
        />
        <upload-btn
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.import)"
          :data="{ areaId: crud.query.areaId, importType: 2 }"
          :upload-fun="listUpload"
          btn-name="清单覆盖导入"
          btn-type="success"
          btn-size="mini"
          class="filter-item"
          @success="uploadSuccess"
        /> -->
        <common-button type="primary" class="filter-item" size="mini" v-if="currentArea && currentArea.id && checkPermission(crud.permission.import)" @click="openUpload(1)">清单增量导入</common-button>
        <common-button type="success" class="filter-item" size="mini" v-if="currentArea && currentArea.id && checkPermission(crud.permission.import)" @click="openUpload(2)">清单覆盖导入</common-button>
        <export-button
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.download)"
          :fn="downloadArtifactTree"
          :params="carryParam"
          class="filter-item"
          :disabled="crud.data.length === 0"
        >
          零构件清单(按构件条件查询)
        </export-button>
        <export-button :fn="downloadArtifactTreeTemplate" class="filter-item" v-permission="crud.permission.templateDownload">
          零构件清单模板
        </export-button>
        <el-popconfirm
          :title="`确认清空【${currentArea.name}】下的【零构件清单】么?`"
          @confirm="deleteArtifact"
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.del)"
        >
          <template #reference>
            <common-button type="danger" size="mini" :loading="deleteLoading" class="filter-item" :disabled="crud.data.length === 0">
              一键清空(按区域)
            </common-button>
          </template>
        </el-popconfirm>
      </template>
      <template #viewLeft>
        <el-tooltip effect="light" :content="`${mismatchList.join(',')}`" placement="top">
          <div class="filter-item">
            <el-tag v-if="mismatchList.length > 0" type="danger" class="filter-item" effect="plain">
              本区域下存在{{ mismatchList.length }}条错误数据,鼠标悬停查看
            </el-tag>
          </div>
        </el-tooltip>
        <common-button type="primary" size="mini" @click="techVisible = true" v-if="checkPermission(crud.permission.techDetail)">
          技术交底
        </common-button>
      </template>
    </crudOperation>
    <common-drawer
      append-to-body
      :before-close="
        () => {
          techVisible = false
        }
      "
      :visible="techVisible"
      title="技术交底(结构)"
      size="80%"
    >
      <template #content>
        <structureTable :table-data="tableData[TechnologyTypeAllEnum.STRUCTURE.V]" :is-show="true" style="margin-top: 20px" />
      </template>
    </common-drawer>
    <common-dialog
      append-to-body
      :close-on-click-modal="false"
      :before-close="
        () => {
          uploadVisible = false
        }
      "
      :visible="uploadVisible"
      :title="importType===1 ? '清单增量导入' : '清单覆盖导入'"
      width="400px"
    >
      <div class="parent-div">
        <span class="parent-span"><span class="title-span">项目模式:</span></span>
        <common-select
          v-model="modeType"
          :options="[projectModeEnum.STRUCTURE,projectModeEnum.STRUCTURE_ASSEMBLE]"
          type="enum"
          size="small"
          placeholder="项目模式"
          style="flex:1;"
          :disabled="!changAble"
          @change="projectModeChange"
        />
     </div>
      <div class="parent-div">
        <span class="parent-span"><span style="color:red;">*</span><span class="title-span">清单模式:</span></span>
        <common-select
          v-model="JDImportType"
          :options="importTypeEnum.ENUM"
          type="enum"
          size="small"
          placeholder="清单模式"
           style="flex:1;"
        />
     </div>
      <div class="parent-div">
        <span class="parent-span title-span">上传清单:</span>
        <upload-btn
          :data="{ areaId: crud.query.areaId, importType: importType, JDImportType: JDImportType }"
          :upload-fun="listUpload"
          :btn-name="importType===1 ? '增量导入' : '覆盖导入'"
          :btn-type="importType===1 ? 'primary' : 'success'"
          btn-size="mini"
          style="float:left;"
          :disabled="!JDImportType"
          @success="uploadSuccess"
        />
     </div>
    </common-dialog>
  </div>
</template>

<script setup>
import { defineProps, ref, computed, watch, defineEmits } from 'vue'
import { listUpload, changAbleProjectMode, changeProjectMode } from '@/api/plan/technical-manage/artifact-tree'

import { projectModeEnum } from '@enum-ms/contract'
import { regHeader } from '@compos/use-crud'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { importTypeEnum } from '@enum-ms/plan'
import {
  downloadArtifactTree,
  downloadArtifactTreeTemplate,
  errorArtifact,
  delArtifactTreeByArea
} from '@/api/plan/technical-manage/artifact-tree'
import { getContractTechInfo } from '@/api/contract/project'
import { isNotBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import { useStore } from 'vuex'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'
import structureTable from '@/views/contract/project-manage/module/enclosure-table/structure-table'

const defaultQuery = {
  artifactName: '',
  artifactSerialNumber: '',
  machinePartSerialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false }
}

const store = useStore()

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const tableData = ref({})
const deleteLoading = ref(false)
const techVisible = ref(false)
const uploadVisible = ref(false)
const importType = ref()
const JDImportType = ref()
const { crud, query, CRUD } = regHeader(defaultQuery)
const emit = defineEmits(['getAreaData'])
const mismatchList = ref([])
const changAble = ref(false)
const modeType = ref()
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
      getTechInfo()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => props.globalProject,
  (val) => {
    if (val) {
      modeType.value = props.globalProject.mode
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

function monomerChange() {
  let areaArr = []
  if (monomerSelectRef.value) {
    const val = monomerSelectRef.value.getOption(query.monomerId) || {}
    if (isNotBlank(val)) {
      if (val.areaSimpleList && val.areaSimpleList.length > 0) {
        areaArr = val.areaSimpleList.filter((v) => v.productType === TechnologyTypeAllEnum.STRUCTURE.V)
      }
    }
  }
  emit('getAreaData', areaArr)
}

function uploadSuccess() {
  uploadVisible.value = false
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

checkProjectModeChangAble()

async function checkProjectModeChangAble() {
  changAble.value = false
  if (!props.projectId) {
    return
  }
  try {
    changAble.value = await changAbleProjectMode(props.projectId) || undefined
  } catch (e) {
    console.log('获取异常构件', e)
  }
}

async function projectModeChange(val) {
  try {
    await changeProjectMode(props.projectId, val)
    crud.notify('项目模式更改成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    store.dispatch('project/fetchUserProjects')
    store.dispatch('project/fetchProjectTree')
  } catch (e) {
    modeType.value = props.globalProject.mode
    console.log('项目模式修改', e)
  }
}

async function getErrorArtifactData() {
  mismatchList.value = []
  try {
    const { content } = await errorArtifact({ areaId: crud.query.areaId })
    if (content && content.length > 0) {
      content.map((v) => {
        mismatchList.value.push(v.serialNumber)
      })
    }
  } catch (e) {
    console.log('获取异常构件', e)
  }
}

async function deleteArtifact() {
  deleteLoading.value = true
  try {
    await delArtifactTreeByArea({ areaId: crud.query.areaId })
    crud.notify('操作成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    uploadSuccess()
    deleteLoading.value = false
  } catch (e) {
    console.log('清空部件', e)
    deleteLoading.value = false
  }
}

async function getTechInfo() {
  try {
    const data = await getContractTechInfo(props.projectId)
    if (isNotBlank(data)) {
      tableData.value = {
        [TechnologyTypeAllEnum.STRUCTURE.V]: data.structureList || []
      }
    }
  } catch (error) {
    console.log('获取技术交底', error)
  }
}

function openUpload(type) {
  importType.value = type
  uploadVisible.value = true
  JDImportType.value = undefined
}
</script>
<style lang="scss" scoped>
.parent-div{
  display:flex;
  margin-bottom:20px;
  height:28px;
}
.parent-span{
  // float:left;
  margin-right:8px;
  width:80px;
  text-align:right;
}
.title-span{
  line-height:28px;
}
</style>
