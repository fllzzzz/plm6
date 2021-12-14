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
        :show-type="2"
        @tab-click="tabClick"
      />
      <common-radio-button
        v-model="query.status"
        :options="processingEnum.ENUM"
        show-option-all
        type="enum"
        style="margin-left:0;margin-right:6px;"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.mame"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px;margin-left:0;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px;"
        class="filter-item"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <rrOperation/>
    </div>
        <crudOperation>
      <template #optRight>
        <upload-btn
          v-if="currentArea && currentArea.id"
          v-permission="crud.permission.importList"
          :data="carryParam"
          :upload-fun="listUpload"
          success-msg="导入成功"
          btn-name="清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <export-button
          v-if="currentArea && currentArea.id"
          :fn="downloadEnclosureData"
          :params="exportParam"
          show-btn-text
          btn-text="清单（按构件条件查询）"
          class="filter-item"
        />
        <export-button
          :fn="downloadEnclosureTemplate"
          :params="{category:crud.query.category}"
          show-btn-text
          btn-text="模板下载"
          class="filter-item"
        />
        <!-- <common-button type="primary" size="mini" @click="checkTech">技术参数</common-button> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import { monomerDetail } from '@/api/plan/monomer'
import { processingEnum } from '@enum-ms/plan'
import { TechnologyTypeEnum } from '@enum-ms/contract'
import uploadBtn from '@/components/file-upload/ExcelUploadBtn'
import { listUpload } from '@/api/plan/technical-manage/enclosure'
import ExportButton from '@comp-common/export-button/index.vue'
import { downloadEnclosureData,downloadEnclosureTemplate } from '@/api/plan/technical-manage/enclosure'

const router = useRouter()

const defaultQuery = {
  name: '', serialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false },
  category: { value: TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V, resetAble: false }
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

const carryParam = computed(()=>{
  return { areaId: crud.query.areaId, category: TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V }
})

const exportParam = computed(()=>{
  const param = { ...crud.query }
  return param
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
