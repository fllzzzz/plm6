<template>
  <div class="app-container manual-pack-wrapper">
    <div class="head-container">
      <div class="manual-pack-common-header" style="display: flex; justify-content: space-between">
        <div>
          <component-radio-button v-model="packType" :options="packTypeEnum.ENUM" type="enum" size="small" class="filter-item" />
          <!-- <project-cascader
            v-model="projectId"
            clearable
            class="filter-item"
            style="width: 270px"
            placeholder="选择项目"
            @change="handleProjectChange"
          /> -->
          <!-- <workshop-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="workshopId"
            placeholder="请选择车间"
            clearable
            style="width: 200px"
            class="filter-item"
          /> -->
          <common-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="workshopId"
            :options="workshopList"
            type="other"
            :data-structure="{ key: 'id', label: 'name', value: 'id' }"
            class="filter-item"
            clearable
            style="width: 200px"
            placeholder="请选择车间"
          />
        </div>
        <el-badge :value="totalBadge" :max="99" :hidden="totalBadge < 1">
          <common-button type="primary" size="mini" :disabled="isEmpty" @click="packVisible = true">打包列表</common-button>
        </el-badge>
        <!-- <common-radio-button
            v-if="packType === packTypeEnum.ENCLOSURE.V"
            type="enum"
            v-model="category"
            :options="mesEnclosureTypeEnum.ENUM"
            showOptionAll
            placeholder="请选择围护类型"
            class="filter-item"
          /> -->
        <!-- <factory-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="factoryId"
            class="filter-item"
            clearable
            style="width: 200px"
          /> -->
        <!-- <monomer-select v-model="monomerId" :default="false" clearable :project-id="globalProjectId" class="filter-item" /> -->
      </div>
      <monomer-select-area-tabs
        v-if="packType === packTypeEnum.STRUCTURE.V || packType === packTypeEnum.MACHINE_PART.V"
        :project-id="globalProjectId"
        @change="fetchMonomerAndArea"
        :default="false"
        :productType="packType"
        needConvert
      />
      <monomer-select-area-select
        v-if="packType !== packTypeEnum.STRUCTURE.V && packType !== packTypeEnum.MACHINE_PART.V"
        :project-id="globalProjectId"
        v-model:areaId="areaId"
        v-model:monomerId="monomerId"
        areaClearable
        clearable
      />
      <area-tabs
        v-if="packType === packTypeEnum.ENCLOSURE.V && areaInfo.length"
        class="filter-item"
        style="width: 100%"
        v-model="batchId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        :show-type="2"
        @tab-click="tabClick"
      />
      <!-- </div> -->
    </div>
    <component
      ref="mainRef"
      :is="currentView"
      :maxHeight="maxHeight - 20"
      :project-id="globalProjectId"
      :workshop-id="workshopId"
      :monomer-id="monomerId"
      :area-id="packType === packTypeEnum.ENCLOSURE.V ? batchId : areaId"
      @add="beforeAddIn"
    />
    <pack-list-drawer v-model:visible="packVisible" :bagId="bagId" :edit-data="editData" @handleSuccess="handleSuccess" />
    <!-- 一物一码 选择弹窗 -->
    <common-dialog
      title="选择一物一码编号"
      v-model="oneCodeVisible"
      :center="false"
      :close-on-click-modal="false"
      width="680px"
      custom-class="code-dialog"
    >
      <template #titleRight>
        <common-button type="primary" size="mini" @click="oneCodeSave">确认</common-button>
        <span>
          <el-checkbox
            v-model="checkAll"
            :indeterminate="isIndeterminate"
            label="全选"
            size="mini"
            border
            style="height: 29px"
            @change="handleCheckAllChange"
          />
        </span>
      </template>
      <one-code-number-list
        v-model="curRowSelect"
        :list="curNumberList"
        :maxHeight="560"
        @change="handleNumberChange"
      ></one-code-number-list>
    </common-dialog>
  </div>
</template>

<script setup>
// import { ElNotification } from 'element-plus'
import { getWorkshopsAllSimple, getEnclosureBatch } from '@/api/mes/common.js'
import { computed, reactive, ref, provide, watch, nextTick, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import { mapGetters } from '@/store/lib'

import { isBlank, isNotBlank } from '@data-type/index'
import { packTypeEnum, packWorkshopTypeEnum } from '@enum-ms/ship-manage'
import { manualPackPM as permission } from '@/page-permission/ship-manage'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

import useMaxHeight from '@compos/use-max-height'
// import factorySelect from '@comp-base/factory-select'
// import workshopSelect from '@comp-mes/workshop-select'
import structureTable from './structure'
import enclosureTable from './enclosure'
import auxiliaryMaterialTable from './auxiliary-material'
import partTable from './part'
import packListDrawer from './pack-list-drawer'
// import projectCascader from '@comp-base/project-cascader.vue'
// import monomerSelect from '@/components-system/plan/monomer-select'
import monomerSelectAreaTabs from './components/monomer-select-area-tabs.vue'
import areaTabs from '@/components-system/plan/area-tabs'
import oneCodeNumberList from '@/components-system/mes/one-code-number-list'

const route = useRoute()
const mainRef = ref()
const { globalProjectId } = mapGetters(['globalProjectId'])
const packType = ref(packTypeEnum.STRUCTURE.V)
// const factoryId = ref()
const workshopId = ref()
// const category = ref()
const monomerId = ref()
const areaId = ref()
const batchId = ref()
const projectId = ref()
const workshopList = ref([])
const areaInfo = ref([])
const defaultTab = ref({})
// 一物一码选择
const oneCodeVisible = ref(false)
const saveOneCodeData = ref()
const curRowSelect = ref([])
const curNumberList = ref([])
const checkAll = ref(false)
const isIndeterminate = ref(false)
// 编辑信息（打包记录页面传过来的参数）
const editData = ref({})
const bagId = ref()
const packVisible = ref(false)
const packData = reactive({
  [packTypeEnum.STRUCTURE.K]: {},
  [packTypeEnum.ENCLOSURE.K]: {},
  [packTypeEnum.MACHINE_PART.K]: {},
  [packTypeEnum.AUXILIARY_MATERIAL.K]: {}
})

onMounted(() => {
  fetWorkshop()
  fetchBatch()
})

const totalBadge = computed(() => {
  let num = 0
  for (const item in packData) {
    num += packData[item] ? Object.keys(packData[item]).length : 0
  }
  return num
})

const { maxHeight } = useMaxHeight({
  wrapperBox: '.manual-pack-wrapper',
  extraBox: ['.manual-pack-common-header', '.head-container'],
  paginate: true
})

provide('packData', packData)
provide('permission', permission)
provide('projectId', globalProjectId)

const routeParams = computed(() => {
  return route.params
})

watch(
  () => globalProjectId.value,
  (newVal, oldVal) => {
    if (isNotBlank(newVal)) {
      packData[packTypeEnum.STRUCTURE.K] = {}
      packData[packTypeEnum.ENCLOSURE.K] = {}
      packData[packTypeEnum.MACHINE_PART.K] = {}
      packData[packTypeEnum.AUXILIARY_MATERIAL.K] = {}
      fetchBatch()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => packType.value,
  (val) => {
    projectId.value = projectId.value ? projectId.value : globalProjectId.value
    if (packType.value === packTypeEnum.ENCLOSURE.V || packType.value === packTypeEnum.AUXILIARY_MATERIAL.V) {
      monomerId.value = undefined
    }
    fetWorkshop()
    fetchBatch()
  }
)

watch(
  () => routeParams,
  (val) => {
    console.log(val, 'val')
    if (isNotBlank(val.value)) {
      // TODO:有项目后解开注释
      // const projectId = val.value.projectId
      // if (projectId !== globalProjectId) {
      //   ElNotification({ title: '编辑失败（同一个项目才能编辑）', type: 'warning', duration: 3500 })
      //   return
      // }
      editData.value = {
        ...val.value,
        data: JSON.parse(val.value.data)
      }
      bagId.value = val.value.id
      const _data = editData.value.data
      packData[packTypeEnum.STRUCTURE.K] = _data.artifactList && _data.artifactList.reduce((obj, item) => ((obj[item.id] = item), obj), {})
      packData[packTypeEnum.MACHINE_PART.K] =
        (_data.partList && _data.partList.reduce((obj, item) => ((obj[item.id] = item), obj), {})) || {}
      packData[packTypeEnum.ENCLOSURE.K] =
        (_data.enclosureList && _data.enclosureList.reduce((obj, item) => ((obj[item.id] = item), obj), {})) || {}
      packData[packTypeEnum.AUXILIARY_MATERIAL.K] =
        (_data.auxiliaryMaterialList && _data.auxiliaryMaterialList.reduce((obj, item) => ((obj[item.id] = item), obj), {})) || {}
      nextTick(() => {
        packVisible.value = true
      })
    }
  },
  { immediate: true, deep: true }
)

watch([monomerId, areaId], () => {
  mainRef?.value?.refresh()
})

async function fetWorkshop() {
  if (packType.value === packTypeEnum.AUXILIARY_MATERIAL.V) return
  try {
    const { content } = await getWorkshopsAllSimple({
      boolEnabledEnum: true,
      type: packType.value === packTypeEnum.ENCLOSURE.V ? packWorkshopTypeEnum.ENCLOSURE_WORKSHOP.V : packWorkshopTypeEnum.MES_WORKSHOP.V
    })
    workshopList.value = content || []
  } catch (e) {
    console.log('获取车间信息失败', e)
  }
}

async function fetchBatch() {
  if (packType.value === packTypeEnum.STRUCTURE.V || packType.value === packTypeEnum.MACHINE_PART.V) return
  try {
    const data = await getEnclosureBatch(globalProjectId.value)
    areaInfo.value = data || []
    // defaultTab.value = {
    //   id: data[0]?.id + '',
    //   name: data[0]?.name
    // }
  } catch (e) {
    console.log('获取围护的批次失败', e)
  }
}

const currentView = computed(() => {
  switch (packType.value) {
    case packTypeEnum.STRUCTURE.V:
      return structureTable
    case packTypeEnum.ENCLOSURE.V:
      return enclosureTable
    case packTypeEnum.MACHINE_PART.V:
      return partTable
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterialTable
    default:
      return ''
  }
})

const isEmpty = computed(() => {
  return (
    isBlank(packData[packTypeEnum.STRUCTURE.K]) &&
    isBlank(packData[packTypeEnum.ENCLOSURE.K]) &&
    isBlank(packData[packTypeEnum.MACHINE_PART.K]) &&
    isBlank(packData[packTypeEnum.AUXILIARY_MATERIAL.K])
  )
})

function handleSuccess() {
  packData[packTypeEnum.STRUCTURE.K] = {}
  packData[packTypeEnum.ENCLOSURE.K] = {}
  packData[packTypeEnum.MACHINE_PART.K] = {}
  packData[packTypeEnum.AUXILIARY_MATERIAL.K] = {}
  mainRef.value.refresh()
  bagId.value = undefined
  editData.value = {}
}

function beforeAddIn(row, packTypeK) {
  if (row.boolOneCode) {
    saveOneCodeData.value = { row, packTypeK }
    curRowSelect.value = []
    curNumberList.value = row.originNumberList
    oneCodeVisible.value = true
    checkAll.value = false
    isIndeterminate.value = false
  } else {
    addIn(row, packTypeK)
  }
}

function handleCheckAllChange() {
  if (checkAll.value) {
    curRowSelect.value = curNumberList.value.map((row) => row.number)
  } else {
    curRowSelect.value = []
  }
}

function handleNumberChange(list) {
  if (list.length === 0) {
    isIndeterminate.value = false
    checkAll.value = false
  } else if (list.length === curNumberList.value.length) {
    isIndeterminate.value = false
    checkAll.value = true
  } else {
    isIndeterminate.value = true
  }
}

function oneCodeSave() {
  const { row, packTypeK } = saveOneCodeData.value
  row.numberList = curRowSelect.value
  row.productQuantity = curRowSelect.value.length
  oneCodeVisible.value = false
  addIn(row, packTypeK)
}

function addIn(row, packTypeK) {
  packData[packTypeK][row.id] = { ...row }
}

// const fetchData = (val) => {
//   console.log(val)
//   monomerId.value = val?.monomerId
// }

function fetchMonomerAndArea(val) {
  monomerId.value = val?.monomerId
  areaId.value = val?.areaId
  // mainRef?.value?.refresh()
}

function tabClick(val) {
  const { name } = val
  batchId.value = name
  mainRef?.value?.refresh()
}

// function handleProjectChange(val) {
//   projectId.value = val || globalProjectId.value
//   mainRef?.value?.refresh()
// }
</script>

<style lang="scss" scoped>
.manual-pack-wrapper {
  > .head-container {
    margin-bottom: 0;
  }
  .app-container {
    padding: 0;
  }
}
</style>
