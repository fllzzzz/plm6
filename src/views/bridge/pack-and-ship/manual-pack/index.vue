<template>
  <div class="app-container manual-pack-wrapper">
    <div>
      <div class="filter-container manual-pack-common-header">
        <div class="filter-left-box">
          <component-radio-button
            v-model="packType"
            :options="packTypeEnum.ENUM"
            :disabledVal="[packTypeEnum.AUXILIARY_MATERIAL.V]"
            type="enum"
            size="small"
            class="filter-item"
          />
          <workshop-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="workshopId"
            placeholder="请选择车间"
            clearable
            style="width: 200px"
            class="filter-item"
          />
          <!-- <factory-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="factoryId"
            class="filter-item"
            clearable
            style="width: 200px"
          /> -->
          <monomer-select v-model="monomerId" :default="false" clearable :project-id="globalProjectId" class="filter-item" />
        </div>
        <div class="filter-right-box">
          <el-badge :value="totalBadge" :max="99" :hidden="totalBadge < 1">
            <common-button type="primary" size="mini" :disabled="isEmpty" @click="packVisible = true">打包列表</common-button>
          </el-badge>
        </div>
      </div>
    </div>
    <component
      ref="mainRef"
      :is="currentView"
      :maxHeight="maxHeight"
      :project-id="globalProjectId"
      :workshop-id="workshopId"
      :monomer-id="monomerId"
      :area-id="areaId"
      :category="category"
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
      </template>
      <one-code-number-list v-model="curRowSelect" :list="curNumberList" :maxHeight="560"></one-code-number-list>
    </common-dialog>
  </div>
</template>

<script setup>
// import { ElNotification } from 'element-plus'
import { computed, reactive, ref, provide, watch, nextTick } from 'vue'
import { useRoute } from 'vue-router'
import { mapGetters } from '@/store/lib'

import { isBlank, isNotBlank } from '@data-type/index'
import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'
import { bridgeManualPackPM as permission } from '@/page-permission/bridge'

import useMaxHeight from '@compos/use-max-height'
// import factorySelect from '@comp-base/factory-select'
import workshopSelect from '@/components-system/bridge/workshop-select'
import boxTable from './box'
import cellTable from './cell'
import auxiliaryMaterialTable from './auxiliary-material'
import packListDrawer from './pack-list-drawer'
import monomerSelect from '@/components-system/plan/monomer-select'
import oneCodeNumberList from '@/components-system/mes/one-code-number-list'

const route = useRoute()
const mainRef = ref()
const { globalProjectId } = mapGetters(['globalProjectId'])
const packType = ref(packTypeEnum.BOX.V)
// const factoryId = ref()
const workshopId = ref()
const category = ref()
const monomerId = ref()
const areaId = ref()
// 一物一码选择
const oneCodeVisible = ref(false)
const saveOneCodeData = ref()
const curRowSelect = ref([])
const curNumberList = ref([])
// 编辑信息（打包记录页面传过来的参数）
const editData = ref({})
const bagId = ref()
const packVisible = ref(false)
const packData = reactive({
  [packTypeEnum.BOX.K]: {},
  [packTypeEnum.CELL.K]: {},
  [packTypeEnum.AUXILIARY_MATERIAL.K]: {}
})

const totalBadge = computed(() => {
  let num = 0
  for (const item in packData) {
    num += packData[item] ? Object.keys(packData[item]).length : 0
  }
  return num
})

const { maxHeight } = useMaxHeight({ wrapperBox: '.manual-pack-wrapper', extraBox: ['.manual-pack-common-header', '.head-container'] })

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
      packData[packTypeEnum.BOX.K] = {}
      packData[packTypeEnum.CELL.K] = {}
      packData[packTypeEnum.AUXILIARY_MATERIAL.K] = {}
    }
  }
)

watch(
  () => routeParams,
  (val) => {
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
      packData[packTypeEnum.BOX.K] = _data.artifactList && _data.artifactList.reduce((obj, item) => ((obj[item.id] = item), obj), {})
      packData[packTypeEnum.CELL.K] =
        (_data.enclosureList && _data.enclosureList.reduce((obj, item) => ((obj[item.id] = item), obj), {})) || {}
      packData[packTypeEnum.AUXILIARY_MATERIAL.K] =
        (_data.auxList && _data.auxList.reduce((obj, item) => ((obj[item.id] = item), obj), {})) || {}
      nextTick(() => {
        packVisible.value = true
      })
      console.log(val.value, editData.value, 'editData')
    }
  },
  { immediate: true, deep: true }
)

const currentView = computed(() => {
  switch (packType.value) {
    case packTypeEnum.BOX.V:
      return boxTable
    case packTypeEnum.CELL.V:
      return cellTable
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterialTable
    default:
      return ''
  }
})

const isEmpty = computed(() => {
  return (
    isBlank(packData[packTypeEnum.BOX.K]) &&
    isBlank(packData[packTypeEnum.CELL.K]) &&
    isBlank(packData[packTypeEnum.AUXILIARY_MATERIAL.K])
  )
})

function handleSuccess() {
  packData[packTypeEnum.BOX.K] = {}
  packData[packTypeEnum.CELL.K] = {}
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
  } else {
    addIn(row, packTypeK)
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
</script>

<style lang="scss" scoped>
.app-container {
  .app-container {
    padding: 0;
  }
}
</style>
