<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <!-- <project-cascader v-model="query.projectId" style="width: 320px" class="filter-item" /> -->
          <!-- <project-visa-select
            v-model="query.projectId"
            class="filter-item"
            style="width: 300px"
            clearable
            placeholder="可选择项目搜索"
            @change="crud.toQuery"
          /> -->
          <project-select
            v-model="query.projectId"
            class="filter-item"
            style="width: 300px"
            :default-id="projectId"
            clearable
            placeholder="可选择项目搜索"
            @projectChange="projectChange"
          />
          <common-radio-button
            v-model="query.type"
            :options="productEnum"
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <!-- <common-radio-button
            v-model="query.type"
            :options="contractSaleTypeEnumArr"
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          /> -->
          <common-radio-button
            v-model="query.status"
            :options="reviewStatusEnum.ENUM"
            showOptionAll
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, inject, computed, watch } from 'vue'
import { projectTypeEnum } from '@enum-ms/contract'
import { contractSaleTypeEnum } from '@enum-ms/mes'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { reviewStatusEnum } from '@enum-ms/common'
import { mapGetters } from '@/store/lib'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectSelect from '@comp-base/project-select'
// import projectCascader from '@comp-base/project-cascader'
// import projectVisaSelect from '@comp-base/project-visa-select'

const { contractSaleTypeEnumArr } = mapGetters('contractSaleTypeEnumArr')
const currentProjectVal = ref({})
const defaultQuery = {
  type: undefined, status: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const projectId = inject('projectId')

// 产品类型
const productEnum = computed(() => {
  // 箱体和建刚不会同时存在
  if (currentProjectVal.value?.projectType === projectTypeEnum.BRIDGE.V) {
    return [bridgeComponentTypeEnum.BOX]
  }
  if (currentProjectVal.value?.projectType === projectTypeEnum.ENCLOSURE.V) {
    return [contractSaleTypeEnum.ENCLOSURE, contractSaleTypeEnum.AUXILIARY_MATERIAL]
  }
  return contractSaleTypeEnumArr.value
})

function projectChange(val) {
  currentProjectVal.value = val
  crud.toQuery()
}

watch(
  productEnum,
  (val) => {
    if (productEnum.value.length) {
      query.type = productEnum.value[0].V
    }
  },
  { immediate: true, deep: true }
)
</script>
