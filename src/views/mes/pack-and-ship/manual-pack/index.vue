<template>
  <div class="app-container">
    <div>
      <div class="filter-container">
        <div class="filter-left-box">
          <common-radio-button v-model="packType" :options="packTypeEnum.ENUM" type="enum" size="small" class="filter-item" />
          <factory-select
            v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
            v-model="factoryId"
            class="filter-item"
            clearable
            style="width: 200px"
          />
          <!-- <monomer-area-cascader
            v-model="monomerAndArea"
            :project-id="globalProjectId"
            :disabled="!globalProjectId"
            :material-type="packType"
            show-all-levels
            check-strictly
            emit-path
            clearable
            filterable
            size="small"
            class="filter-item"
            style="width:355px"
            @change="handleMonomerAreaChange"
          /> -->
        </div>
        <div class="filter-right-box">
          <common-button type="primary" size="mini" :disabled="isEmpty" @click="packVisible = true">打包列表</common-button>
        </div>
      </div>
    </div>
    <component
      ref="mainRef"
      :is="currentView"
      :project-id="globalProjectId"
      :factory-id="factoryId"
      :monomer-id="monomerId"
      :district-id="districtId"
      @add="addIn"
    />
    <pack-list-drawer v-model:visible="packVisible" :bagId="bagId" :edit-data="editData" @handleSuccess="handleSuccess" />
  </div>
</template>

<script setup>
// import { ElNotification } from 'element-plus'
import { computed, reactive, ref, provide, watch, nextTick } from 'vue'
import { useRoute } from 'vue-router'
import { mapGetters } from '@/store/lib'

import { isBlank, isNotBlank } from '@data-type/index'
import { packTypeEnum } from '@enum-ms/mes'

import factorySelect from '@comp-base/factory-select'
import structureTable from './structure'
import enclosureTable from './enclosure'
import auxiliaryMaterialTable from './auxiliary-material'
import packListDrawer from './pack-list-drawer'

const route = useRoute()
const permission = {
  pack: ['manualPack:pack']
}
const mainRef = ref()
const { globalProjectId } = mapGetters(['globalProjectId'])
const packType = ref(packTypeEnum.STRUCTURE.V)
const factoryId = ref()
const monomerId = ref()
const districtId = ref()
// 编辑信息（打包记录页面传过来的参数）
const editData = ref({})
const bagId = ref()
const packVisible = ref(false)
const packData = reactive({
  [packTypeEnum.STRUCTURE.K]: {},
  [packTypeEnum.ENCLOSURE.K]: {},
  [packTypeEnum.AUXILIARY_MATERIAL.K]: {}
})

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