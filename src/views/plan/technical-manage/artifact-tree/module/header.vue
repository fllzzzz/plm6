<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        @getAreaInfo="getAreaInfo"
      />
      <area-tabs
        class="filter-item"
        style="width: calc(100% - 230px);"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-input
        v-model="query.artifactName"
        size="small"
        placeholder="输入构件名称搜索"
        style="width: 170px;margin-left:0;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.artifactSerialNumber"
        size="small"
        placeholder="输入构件编号搜索"
        style="width: 170px;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.machinePartSerialNumber"
        size="small"
        placeholder="输入零件编号搜索"
        style="width: 170px;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          v-if="currentArea && currentArea.id"
          v-permission="crud.permission.importList"
          :data="{ areaId: crud.query.areaId }"
          :upload-fun="listUpload"
          btn-name="零构件清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <!-- <el-tooltip
          class="item"
          effect="dark"
          :content="`上传零构件清单: \n
          1. 新清单上传会直接清空当前区域的老清单数据,
             不会保留老清单任何数据；\n`"
          placement="top"
        >
          <upload-btn
            v-if="currentArea && currentArea.id"
            v-permission="permission.importList"
            :data="{ areaId: query.areaId }"
            :upload-fun="listUpload"
            btn-name="零构件清单导入"
            btn-type="primary"
            btn-size="mini"
            class="filter-item"
            @success="crud.toQuery"
          />
        </el-tooltip>
        <export-button
          v-if="currentArea && currentArea.id"
          :fn="downloadArtifactTree"
          :params="carryParam"
          show-btn-text
          btn-text="零构件清单（按构件条件查询）"
          class="filter-item"
        />
        <export-button
          :fn="downloadArtifactTreeTemplate"
          show-btn-text
          btn-text="零构件清单模板"
          class="filter-item"
        /> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@/components/file-upload/ExcelUploadBtn'
import { monomerDetail } from '@/api/plan/monomer'
import { listUpload } from '@/api/plan/technical-manage/artifact-tree'

const router = useRouter()

const defaultQuery = {
  artifactName: '', 
  artifactSerialNumber: '', 
  machinePartSerialNumber: '',
  monomerId: undefined,
  areaId: undefined,
  projectId: undefined
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'no', label: 'name', value: 'no' }
const typeOption = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

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

</script>
