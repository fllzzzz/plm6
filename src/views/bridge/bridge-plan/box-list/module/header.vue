<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :productType="TechnologyTypeAllEnum.BRIDGE.V"
        :show-tips="areaInfo.length <= 0"
        @getAreaInfo="getAreaInfo"
      />
      <area-tabs
        class="filter-item"
        :style="areaInfo.length > 0 ? 'width:calc(100% - 230px)' : 'width:calc(100% - 380px)'"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-input v-model="query.serialNumber" size="small" placeholder="输入编号搜索" style="width: 170px" class="filter-item" clearable />
      <el-input v-model="query.specification" size="small" placeholder="输入规格搜索" style="width: 170px" class="filter-item" clearable />
      <el-input v-model="query.material" size="small" placeholder="输入材质搜索" style="width: 170px" class="filter-item" clearable />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
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
        />
        <export-button
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.download)"
          :fn="downloadBoxCell"
          :params="exportParam"
          class="filter-item"
          :disabled="crud.data.length === 0"
        >
          分段单元清单(按条件查询)
        </export-button>
        <export-button :fn="downloadBoxCellTemplate" class="filter-item" v-permission="crud.permission.templateDownload">
          分段单元清单模板
        </export-button>
        <el-popconfirm
          :title="`确认清空【${currentArea.name}】下的【部件清单】么?`"
          @confirm="deleteByArea"
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.del)"
        >
          <template #reference>
            <common-button type="danger" size="mini" :loading="deleteLoading" class="filter-item" :disabled="crud.data.length === 0">
              一键清空(按区域)
            </common-button>
          </template>
        </el-popconfirm>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import { listUpload, downloadBoxCellTemplate, downloadBoxCell, delBoxCellByArea } from '@/api/plan/technical-manage/bridge/box-list'
import ExportButton from '@comp-common/export-button/index.vue'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import rrOperation from '@crud/RR.operation'
// import { downloadAssemble } from '@/api/plan/technical-manage/assembly'
// import { downloadAssemble, downloadAssembleTemplate, delAssemblyByArea, assembleError } from '@/api/plan/technical-manage/assembly'
// import { projectModeEnum } from '@enum-ms/contract'
// import { ElMessageBox } from 'element-plus'

const defaultQuery = {
  name: '',
  serialNumber: '',
  specification: '',
  material: '',
  // monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const deleteLoading = ref(false)
// const errorList = ref([])
const { crud, query, CRUD } = regHeader(defaultQuery)
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

const exportParam = computed(() => {
  const param = { ...crud.query }
  return param
})

// const AddParam = computed(() => {
//   return { areaId: crud.query.areaId, importType: 1 }
// })

// const carryParam = computed(() => {
//   return { areaId: crud.query.areaId, importType: 2 }
// })

function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  uploadSuccess()
}

function getAreaInfo(val) {
  console.log(areaInfo.value)
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

async function deleteByArea() {
  deleteLoading.value = true
  try {
    await delBoxCellByArea({ areaId: crud.query.areaId })
    crud.notify('操作成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    uploadSuccess()
    deleteLoading.value = false
  } catch (e) {
    console.log('清空分段单元', e)
    deleteLoading.value = false
  }
}

function uploadSuccess() {
  crud.toQuery()
}
</script>
