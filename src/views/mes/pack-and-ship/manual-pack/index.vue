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
          <common-radio-button
            v-if="packType === packTypeEnum.ENCLOSURE.V"
            type="enum"
            v-model="category"
            :options="mesEnclosureTypeEnum.ENUM"
            showOptionAll
            placeholder="请选择围护类型"
            class="filter-item"
          />
          <factory-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="factoryId"
            class="filter-item"
            clearable
            style="width: 200px"
          />
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
      :factory-id="factoryId"
      :monomer-id="monomerId"
      :area-id="areaId"
      :category="category"
      @add="beforeAddIn"
    />
    <pack-list-drawer v-model:visible="packVisible" :bagId="bagId" :edit-data="editData" @handleSuccess="handleSuccess" />
    <!-- 一物一码 选择弹窗 -->
    <common-dialog title="选择一物一码编号" v-model="oneCodeVisible" :center="false" :close-on-click-modal="false" width="450px">
      <template #titleRight>
        <common-button type="primary" size="mini" @click="oneCodeSave">确认</common-button>
      </template>
      <one-code-number-list v-model="curRowSelect" :list="curNumberList"></one-code-number-list>
    </common-dialog>
  </div>
</template>

<script setup>
// import { ElNotification } from 'element-plus'
import { computed, reactive, ref, provide, watch, nextTick } from 'vue'
import { useRoute } from 'vue-router'
import { mapGetters } from '@/store/lib'

import { isBlank, isNotBlank } from '@data-type/index'
import { packTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import { manualPackPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import factorySelect from '@comp-base/factory-select'
import structureTable from './structure'
import enclosureTable from './enclosure'
import auxiliaryMaterialTable from './auxiliary-material'
import packListDrawer from './pack-list-drawer'
import monomerSelect from '@/components-system/plan/monomer-select'
import oneCodeNumberList from '@/components-system/mes/one-code-number-list'

const route = useRoute()
const mainRef = ref()
const { globalProjectId } = mapGetters(['globalProjectId'])
const packType = ref(packTypeEnum.STRUCTURE.V)
const factoryId = ref()
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
  [packTypeEnum.STRUCTURE.K]: {},
  [packTypeEnum.ENCLOSURE.K]: {},
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
  () => routeParams,
  (val) => {
    if (isNotBlank(val.value)) {
      // TODO:有项目后解开注释
      //  const projectId = val.value.projectId
      //  if (projectId !== globalProjectId) {
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
      packData[packTypeEnum.ENCLOSURE.K] =
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
    case packTypeEnum.STRUCTURE.V:
      return structureTable
    case packTypeEnum.ENCLOSURE.V:
      return enclosureTable
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
    isBlank(packData[packTypeEnum.AUXILIARY_MATERIAL.K])
  )
})

function handleSuccess() {
  packData[packTypeEnum.STRUCTURE.K] = {}
  packData[packTypeEnum.ENCLOSURE.K] = {}
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
