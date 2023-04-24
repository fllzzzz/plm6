<template>
  <div>
    <div v-show="crud.searchToggle">
      <div style="margin-bottom:5px;">
        <span v-if="tipsShow" style="font-size:12px;color:red;margin-left:10px;">*当前项目下未创建计划</span>
        <area-tabs
          class="filter-item"
          :style="areaInfo.length > 0 ? 'width:calc(100% - 230px)' : 'width:calc(100% - 380px)'"
          v-model="query.enclosurePlanId"
          :area-info="areaInfo"
          :default-tab="defaultTab"
          :show-type="2"
          @tab-click="tabClick"
        />
      </div>
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
        <upload-btn
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.import)"
          :data="addParam"
          :upload-fun="listUpload"
          success-msg="导入成功"
          btn-name="清单增量导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <upload-btn
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.import)"
          :data="carryParam"
          :upload-fun="listUpload"
          success-msg="导入成功"
          btn-name="清单覆盖导入"
          btn-type="success"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <common-button
          type="primary"
          size="mini"
          @click="crud.toAdd"
          class="filter-item"
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.save)"
          >手动添加</common-button
        >
        <export-button
          v-permission="crud.permission.templateDownload"
          :fn="downloadEnclosureTemplate"
          :params="{ category: crud.query.category }"
          class="filter-item"
        >
          模板下载
        </export-button>
        <export-button
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.download)"
          :fn="downloadEnclosureData"
          :params="exportParam"
          :disabled="crud.data.length === 0"
          class="filter-item"
        >
          清单(按条件查询)
        </export-button>
        <el-popconfirm
          :title="`确认清空【${currentArea.name}】下的【围护清单】么?`"
          @confirm="deleteEnclosure"
          v-if="currentArea && currentArea.id && checkPermission(crud.permission.del)"
        >
          <template #reference>
            <common-button type="danger" size="mini" class="filter-item">一键清空(按区域)</common-button>
          </template>
        </el-popconfirm>
        <el-tag type="success" size="medium" effect="plain" class="filter-item" v-if="sumData.totalLength">
          <span>{{ `总长度:${sumData.totalLength.toFixed(DP.MES_ENCLOSURE_L__M)}m` }}</span>
          <span>{{ ` | 总数量:${sumData.totalQuantity}张` }}</span>
        </el-tag>
      </template>
      <template #viewLeft>
        <common-button
          type="primary"
          size="mini"
          @click="techVisible = true"
          v-if="query.category !== TechnologyTypeAllEnum.BENDING.V && crud.query.projectId && checkPermission(crud.permission.techDetail)"
          >技术交底配置（已{{ enabledEnum.V?.[technicalTypeStatus]?.L }}）</common-button
        >
        <zip-upload-btn
          ref="changeFileRef"
          v-if="query.category === TechnologyTypeAllEnum.BENDING.V && checkPermission(crud.permission.draw) && query.enclosurePlanId"
          :upload-fun="uploadBendingZip"
          :data="{ enclosurePlanId: query.enclosurePlanId }"
          :btn-name="'批量上传图纸'"
          :btn-type="'warning'"
          :btn-size="'mini'"
          :accept="'.zip,.jpg,.jpeg,.png'"
          class="filter-item"
          @success="crud.toQuery"
          :tip="'.jpg,.jpeg,.png'"
        />
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
      :title="`技术交底(${query.category ? TechnologyTypeAllEnum.VL[query.category] : ''})`"
      size="80%"
    >
      <template #content>
        <component :is="currentView" :table-data="tableData[query.category]" :is-show="true" style="margin-top: 20px" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { allProjectPlan } from '@/api/enclosure/enclosure-plan/area'
import { uploadBendingZip, downloadEnclosureData, downloadEnclosureTemplate, listUpload, getTotalSum, delEnclosureByArea } from '@/api/plan/technical-manage/enclosure'
import { watch, ref, computed, inject, defineProps } from 'vue'
import { regHeader } from '@compos/use-crud'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { enabledEnum } from '@enum-ms/common'
import { DP } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'
import pressedSupportTable from '@/views/contract/project-manage/module/enclosure-table/pressed-support-table'
import sandwichTable from '@/views/contract/project-manage/module/enclosure-table/sandwich-table'
import pressedColorTable from '@/views/contract/project-manage/module/enclosure-table/pressed-color-table'
import trussSupportTable from '@/views/contract/project-manage/module/enclosure-table/truss-support-table'
import zipUploadBtn from '@/views/plan/technical-data-manage/components/drawing-upload-btn'

const defaultQuery = {
  name: undefined,
  serialNumber: undefined,
  enclosurePlanId: { value: undefined, resetAble: false },
  category: { value: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V, resetAble: false },
  thickness: undefined,
  plate: undefined
}

const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query, CRUD } = regHeader(defaultQuery)
const techVisible = ref(false)

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
  },
  typeOption: {
    type: Array,
    default: () => []
  }
})

// const AllAreaInfo = ref([])
const tipsShow = ref(false)
const sumData = ref({})
const technicalTypeStatus = inject('technicalTypeStatus') // 技术交底状态

const currentView = computed(() => {
  switch (crud.query.category) {
    case TechnologyTypeAllEnum.PROFILED_PLATE.V:
      return pressedColorTable
    case TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V:
      return trussSupportTable
    case TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V:
      return pressedSupportTable
    case TechnologyTypeAllEnum.SANDWICH_BOARD.V:
      return sandwichTable
    default:
      return ''
  }
})

watch(
  () => props.projectId,
  (val) => {
    getAllProjectPlan()
  },
  { deep: true, immediate: true }
)

// watch(
//   () => props.typeOption,
//   (val) => {
//     query.category = val?.length > 0 ? val[0].no : undefined
//   },
//   { deep: true, immediate: true }
// )

const addParam = computed(() => {
  return { enclosurePlanId: crud.query.enclosurePlanId, category: query.category, importType: 1 }
})
const carryParam = computed(() => {
  return { enclosurePlanId: crud.query.enclosurePlanId, category: query.category, importType: 2 }
})

const exportParam = computed(() => {
  const param = { ...crud.query }
  return param
})

// function categoryChange() {
//   choseInfo()
//   getData()
//   emit('categoryChange')
//   crud.data = []
//   crud.toQuery()
// }

// function choseInfo() {
//   currentArea.value = {}
//   areaInfo.value =
//     crud.query.category && AllAreaInfo.value.length > 0
//       ? AllAreaInfo.value.filter((v) => v.productType === crud.query.category)
//       : AllAreaInfo.value
//   if (areaInfo.value.length > 0) {
//     defaultTab.value = {
//       id: areaInfo.value[0].id + '',
//       name: areaInfo.value[0].name
//     }
//   } else {
//     defaultTab.value = {}
//   }
//   currentArea.value = { ...defaultTab.value }
// }
// function tabClick(val) {
//   const { name, label } = val
//   currentArea.value = {
//     id: name,
//     name: label
//   }
//   crud.toQuery()
// }

function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  getData()
  crud.toQuery()
}

async function getAllProjectPlan() {
  areaInfo.value = []
  currentArea.value = {}
  defaultTab.value = {}
  tipsShow.value = false
  if (props.projectId) {
    try {
      const data = await allProjectPlan(props.projectId) || []
      areaInfo.value = data.filter(v => v.category === crud.query.category) || []
      if (areaInfo.value && areaInfo.value.length > 0) {
        defaultTab.value = {
          id: areaInfo.value[0].id + '',
          name: areaInfo.value[0].name
        }
        getData()
      } else {
        tipsShow.value = true
      }
    } catch (e) {
      console.log('获取项目所有计划', e)
    }
  } else {
    sumData.value = {}
  }
}

// function getAreaInfo(val) {
//   AllAreaInfo.value = val || []
//   choseInfo()
//   getData()
// }

async function getData() {
  if (crud.query.enclosurePlanId && crud.query.category) {
    try {
      sumData.value = await getTotalSum({ planId: crud.query.enclosurePlanId, category: crud.query.category })
    } catch (e) {
      console.log('获取围护汇总', e)
    }
  } else {
    sumData.value = {}
  }
}

async function deleteEnclosure() {
  try {
    await delEnclosureByArea(crud.query.enclosurePlanId)
    crud.notify('操作成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('清空区域下围护', e)
  }
}
</script>
