<template>
  <div>
    <div v-show="crud.searchToggle">
      <div>
        <common-radio-button
          v-model="query.category"
          :options="typeOption"
          :type="'other'"
          :dataStructure="typeProp"
          class="filter-item"
          @change="categoryChange"
        />
      </div>
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
        :show-type="2"
        @tab-click="tabClick"
      />
      <el-input
        v-model="query.name"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.plate"
        size="small"
        placeholder="输入板型搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
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
        v-model="query.thickness"
        size="small"
        placeholder="输入厚度搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optRight>
        <common-radio-button
          v-if="currentArea && currentArea.id"
          v-model="query.createType"
          :options="enclosureCreateTypeEnum.ENUM"
          type="enum"
          size="small"
          class="filter-item"
        />
        <template v-if="query.createType===enclosureCreateTypeEnum.UPLOAD.V">
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
            :fn="downloadEnclosureTemplate"
            :params="{ category: crud.query.category }"
            show-btn-text
            btn-text="模板下载"
            class="filter-item"
          />
       </template>
        <template v-else>
          <common-button type="success" size="mini" @click="emit('tableAdd')" class="filter-item" v-if="currentArea && currentArea.id">添加一行</common-button>
        </template>
        <export-button
          v-if="currentArea && currentArea.id"
          :fn="downloadEnclosureData"
          :params="exportParam"
          show-btn-text
          btn-text="清单（按条件查询）"
          class="filter-item"
        />
      </template>
      <template #viewLeft>
        <common-button type="primary" size="mini" @click="techVisible=true" v-if="query.category!==TechnologyTypeAllEnum.BENDING.V">技术交底</common-button>
      </template>
    </crudOperation>
    <common-drawer
      append-to-body
      :before-close="()=>{techVisible=false}"
      :visible="techVisible"
      title="技术交底"
      size="80%"
    >
      <template #content>
       <component
        :is="currentView"
        :table-data="tableData[query.category]"
        :is-show="true"
        style="margin-top:20px;"
      />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { defineProps, ref, computed, defineEmits, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import { enclosureCreateTypeEnum } from '@enum-ms/plan'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import { listUpload } from '@/api/plan/technical-manage/enclosure'
import ExportButton from '@comp-common/export-button/index.vue'
import { downloadEnclosureData, downloadEnclosureTemplate } from '@/api/plan/technical-manage/enclosure'
import pressedSupportTable from '@/views/contract/project-manage/module/enclosure-table/pressed-support-table'
import sandwichTable from '@/views/contract/project-manage/module/enclosure-table/sandwich-table'
import pressedColorTable from '@/views/contract/project-manage/module/enclosure-table/pressed-color-table'
import trussSupportTable from '@/views/contract/project-manage/module/enclosure-table/truss-support-table'
import { isNotBlank } from '@data-type/index'

const defaultQuery = {
  name: undefined,
  serialNumber: undefined,
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  category: { value: undefined, resetAble: false },
  createType: { value: enclosureCreateTypeEnum.UPLOAD.V, resetAble: false },
  thickness: undefined,
  plate: undefined
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const techVisible = ref(false)
const emit = defineEmits(['tableAdd', 'categoryChange'])

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  tableData: {
    type: Object,
    default: () => {}
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})
const typeProp = { key: 'no', label: 'name', value: 'no' }
const AllAreaInfo = ref([])
const typeOption = ref([])
const techOptions = [
  {
    name: '压型彩板',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '夹芯板',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  }
]
const currentView = computed(() => {
  switch (crud.query.category) {
    case TechnologyTypeAllEnum.PROFILED_PLATE.V : return pressedColorTable
    case TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V: return trussSupportTable
    case TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V : return pressedSupportTable
    case TechnologyTypeAllEnum.SANDWICH_BOARD.V: return sandwichTable
    default: return ''
  }
})
watch(
  () => props.globalProject,
  (val) => {
    typeOption.value = []
    if (isNotBlank(val)) {
      techOptions.forEach(v => {
        if (val.businessType === businessTypeEnum.MACHINING.V) {
          if (val.projectContentList.findIndex((k) => Number(k.no) === v.no) > -1) {
            typeOption.value.push(v)
          }
        } else if (val.businessType === businessTypeEnum.INSTALLATION.V) {
          val.projectContentList.forEach(k => {
            if (k.childrenList && k.childrenList.length > 0 && k.childrenList.findIndex((value) => Number(value.no) === v.no) > -1) {
              typeOption.value.push(v)
            }
          })
        }
      })
      if (typeOption.value.findIndex((k) => k.alias === 'ENCLOSURE') > -1) {
        const optionVal = {
          name: '折边件',
          alias: 'ENCLOSURE',
          no: TechnologyTypeAllEnum.BENDING.V
        }
        typeOption.value.push(optionVal)
      }
      query.category = typeOption.value.length > 0 ? typeOption.value[0].no : undefined
    }
  },
  { deep: true, immediate: true }
)
const carryParam = computed(() => {
  return { areaId: crud.query.areaId, category: query.category }
})

const exportParam = computed(() => {
  const param = { ...crud.query }
  return param
})

function categoryChange() {
  choseInfo()
  emit('categoryChange')
  crud.data = []
  crud.toQuery()
}

function choseInfo() {
  areaInfo.value = crud.query.category && AllAreaInfo.value.length > 0 ? AllAreaInfo.value.filter(v => v.productType === crud.query.category) : AllAreaInfo.value
  if (areaInfo.value.length > 0) {
    defaultTab.value = {
      id: areaInfo.value[0].id + '',
      name: areaInfo.value[0].name
    }
  } else {
    defaultTab.value = {
    }
  }
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
  AllAreaInfo.value = val || []
  choseInfo()
}

</script>
