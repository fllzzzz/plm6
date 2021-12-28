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
        style="width: calc(100% - 230px)"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.material"
        size="small"
        placeholder="输入材质搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optRight>
        <upload-btn
          v-if="currentArea && currentArea.id"
          v-permission="crud.permission.importList"
          :data="carryParam"
          :upload-fun="listUpload"
          success-msg="导入成功"
          btn-name="组立清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <export-button
          v-if="currentArea && currentArea.id"
          :fn="downloadAssemble"
          :params="exportParam"
          show-btn-text
          btn-text="下载组立清单"
          class="filter-item"
        />
        <export-button :fn="downloadAssembleTemplate" show-btn-text btn-text="组立清单模板" class="filter-item" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import { listUpload } from '@/api/plan/technical-manage/assembly'
import ExportButton from '@comp-common/export-button/index.vue'
import { downloadAssemble, downloadAssembleTemplate } from '@/api/plan/technical-manage/assembly'

const defaultQuery = {
  name: '',
  serialNumber: '',
  specification: '',
  material: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const exportParam = computed(() => {
  const param = { ...crud.query }
  return param
})

const carryParam = computed(() => {
  return { areaId: crud.query.areaId }
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
