<template>
  <div class="app-container">
    <div class="head-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <project-visa-select
            v-model="projectId"
            :default-id="globalProjectId"
            class="filter-item"
            style="width: 300px"
            placeholder="可选择项目搜索"
            @projectChange="projectChange"
          />
          <common-radio-button
            v-model="productType"
            :options="contractSaleTypeEnum.ENUM"
            default
            type="enumSL"
            size="small"
            class="filter-item"
            @change="()=>{if(productType===contractSaleTypeEnum.AUXILIARY_MATERIAL.V){monomerId=undefined;areaId=undefined}}"
          />
          <template v-if="productType!==contractSaleTypeEnum.AUXILIARY_MATERIAL.V && productType!==contractSaleTypeEnum.ENCLOSURE.V">
            <monomer-select
              v-model="monomerId"
              :project-id="projectId"
              class="filter-item"
              clearable
              @change="handleMonomerChange"
              @getAreaInfo="getAreaInfo"
            />
            <common-select
              v-model="areaId"
              :options="areaInfo"
              type="other"
              :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
              size="small"
              clearable
              placeholder="请选择区域"
              class="filter-item"
              style="width:200px;"
              @change="fetchCost"
            />
          </template>
          <template v-if="productType===contractSaleTypeEnum.ENCLOSURE.V">
            <common-radio-button
              v-model="category"
              :options="typeOption"
              :type="'other'"
              :dataStructure="{ key: 'no', label: 'name', value: 'no' }"
              class="filter-item"
              @change="categoryChange"
            />
            <common-select
              v-model="enclosurePlanId"
              :options="enclosureAreaInfo"
              type="other"
              :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
              size="small"
              clearable
              placeholder="请选择围护计划"
              class="filter-item"
              style="width:200px;"
              @change="fetchCost"
            />
          </template>
        </div>
        <div class="filter-right-box">
          <el-badge v-if="checkPermission(permission.list)" :value="modifyCount" :hidden="modifyCount <= 0">
            <common-button class="filter-item" size="mini" type="info" @click="modifyVisible = true">变更记录</common-button>
          </el-badge>
        </div>
      </div>
      <div>
        <el-row v-if="checkPermission(permission.cost) && productType!==contractSaleTypeEnum.ENCLOSURE.V" :gutter="20" class="panel-group" style="margin-bottom:10px;">
          <el-col :span="8">
            <Panel name="项目造价（元）" text-color="#626262" num-color="#1890ff" :end-val="projectCost || 0" :precision="2" />
          </el-col>
          <el-col :span="8">
            <Panel name="单体造价（元）" text-color="#626262" num-color="#1890ff" :end-val="monomerCost || 0" :precision="2" />
          </el-col>
          <el-col :span="8">
            <Panel name="选定区域造价（元）" text-color="#626262" num-color="#1890ff" :end-val="areaCost || 0" :precision="2" />
          </el-col>
        </el-row>
      </div>
    </div>
    <component :is="currentView" ref="domRef" @refresh-count="fetchModifyCount" :category="category" />
    <!-- 商务变更记录 -->
    <common-drawer
      append-to-body
      :close-on-click-modal="false"
      :visible="modifyVisible"
      title="变更记录"
      size="80%"
      :before-close="
        () => {
          modifyVisible = false
        }
      "
    >
      <template #content>
        <modifyRecord  @refresh-data="refreshData" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { cost, priceModifyCount } from '@/api/contract/sales-manage/price-manage/common'
import { allProjectPlan } from '@/api/enclosure/enclosure-plan/area'
import { ref, computed, onMounted, provide } from 'vue'
import { mapGetters } from '@/store/lib'
import { priceManagePM as permission } from '@/page-permission/contract'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { contractSaleTypeEnum } from '@enum-ms/mes'
import { debounce } from '@/utils'
import { isBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import { isNotBlank } from '@data-type/index'

import monomerSelect from '@/components-system/plan/monomer-select'
import structure from './structure'
import enclosure from './enclosure'
import auxiliaryMaterial from './auxiliary-material'
import machinePart from './machine-part'
import modifyRecord from './price-modify-list/index'
import projectVisaSelect from '@comp-base/project-visa-select'
import Panel from '@/components/Panel'

// 当前显示组件
const currentView = computed(() => {
  switch (productType.value) {
    case contractSaleTypeEnum.ENCLOSURE.V:
      return enclosure
    case contractSaleTypeEnum.MACHINE_PART.V:
      return machinePart
    case contractSaleTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterial
    default:
      return structure
  }
})

const { globalProjectId } = mapGetters(['globalProjectId'])

const domRef = ref()
const projectId = ref()
const productType = ref()
const modifyCount = ref(0)
const costLoading = ref(false)
const projectCost = ref(0)
const monomerCost = ref(0)
const areaCost = ref(0)
const monomerId = ref()
const areaId = ref()
const modifyVisible = ref(false)
const areaInfo = ref([])
const currentProjectVal = ref({})
const allEnclosureAreaInfo = ref([])
const enclosureAreaInfo = ref([])
const enclosurePlanId = ref()
const category = ref()
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
  },
  {
    name: '折边件',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  }
]

provide('monomerId', monomerId)
provide('areaId', areaId)
provide('projectId', projectId)
provide('modifyVisible', modifyVisible)
provide('enclosurePlanId', enclosurePlanId)

onMounted(() => {
  handleProjectChange()
})

// 项目变动
function handleProjectChange() {
  fetchCost()
  fetchModifyCount()
}

// 单体变动
function handleMonomerChange() {
  fetchCost()
}

function projectChange(val) {
  currentProjectVal.value = val
  typeOption.value = []
  enclosureAreaInfo.value = []
  enclosurePlanId.value = undefined
  if (isNotBlank(currentProjectVal.value)) {
    techOptions.forEach((v) => {
      if (val.projectContentList.findIndex((k) => Number(k.no) === v.no) > -1) {
        typeOption.value.push(v)
      }
    })
    if (typeOption.value.length > 0) {
      category.value = typeOption.value[0].no
      getAllProjectPlan()
    }
  }
}

function categoryChange(val) {
  enclosurePlanId.value = undefined
  enclosureAreaInfo.value = allEnclosureAreaInfo.value.filter(v => v.category === category.value) || []
  if (enclosureAreaInfo.value && enclosureAreaInfo.value.length > 0) {
    enclosurePlanId.value = enclosureAreaInfo.value[0].id
  }
}

async function getAllProjectPlan() {
  try {
    const data = await allProjectPlan(currentProjectVal.value.id) || []
    allEnclosureAreaInfo.value = data
    enclosureAreaInfo.value = allEnclosureAreaInfo.value.filter(v => v.category === category.value) || []
    if (enclosureAreaInfo.value && enclosureAreaInfo.value.length > 0) {
      enclosurePlanId.value = enclosureAreaInfo.value[0].id
    }
  } catch (e) {
    console.log('获取围护项目所有计划', e)
  }
}

function getAreaInfo(val) {
  fetchCost()
  areaInfo.value = val
}
// 获取项目造价
const fetchCost = debounce(async function () {
  if (!checkPermission(permission.cost) || isBlank(projectId.value) || productType.value === contractSaleTypeEnum.ENCLOSURE.V) {
    projectCost.value = 0
    monomerCost.value = 0
    areaCost.value = 0
    return
  }
  try {
    costLoading.value = true
    const params = {
      projectId: projectId.value,
      monomerId: monomerId.value,
      areaId: areaId.value
    }
    const { monomerPrice, projectPrice, areaPrice } = await cost(params)
    projectCost.value = projectPrice || 0
    monomerCost.value = monomerPrice || 0
    areaCost.value = areaPrice || 0
  } catch (error) {
    console.log(error)
  } finally {
    costLoading.value = false
  }
}, 100, false)

// 获取待审核数量
const fetchModifyCount = debounce(async function () {
  if (!checkPermission(permission.list)) return
  try {
    modifyCount.value = await priceModifyCount()
  } catch (error) {
    console.log('变更记录未审核数量', error)
  }
}, 100, false)

// 刷新数据
function refreshData() {
  handleProjectChange()
  domRef.value.refresh()
}
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}

::v-deep(.wrong-td) {
  .cell {
    &:after {
      content: '';
      position: absolute;
      z-index: 1;
      background-color: #ff000021;
      top: 0;
      right: 0;
      bottom: 0;
      left: 0;
      pointer-events: none; // 穿透
    }
  }
}

::v-deep(.el-table.businessTable) {
  th,
  td {
    padding: 0;
  }
  .cell {
    line-height: 46px;
    padding:0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .el-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .el-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}
</style>
