<template>
  <common-radio-button
    v-model="queryVO.projectWarehouseType"
    :options="projectWarehouseTypeEnum.ENUM"
    default
    type="enum"
    size="small"
    class="filter-item"
    @change="toQuery"
  />
  <common-radio-button
    v-model="queryVO.materialIsWhole"
    :options="materialIsWholeEnum.ENUM"
    show-option-all
    type="enum"
    size="small"
    class="filter-item"
    @change="toQuery"
  />
  <slot name="afterProjectWarehouseType" />
  <factory-select v-model="queryVO.factoryId" placeholder="工厂" class="filter-item" @change="toQuery" />
  <warehouse-select
    v-model="queryVO.warehouseId"
    :factory-id="queryVO.factoryId"
    :basic-class="props.basicClass"
    placeholder="存储位置"
    class="filter-item"
    show-all
    @change="toQuery"
  />
  <br />
  <material-cascader
    v-model="queryVO.classifyId"
    :basic-class="props.basicClass"
    separator=" > "
    check-strictly
    show-all-levels
    clearable
    size="small"
    class="filter-item"
    style="width: 300px"
    placeholder="可选择/输入科目、编码搜索"
    @change="toQuery"
  />
  <component :is="comp" :query="query" :basic-class="props.basicClass" @to-query="toQuery" />
</template>

<script setup>
import { defineProps, computed, watchEffect, ref } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { projectWarehouseTypeEnum, materialIsWholeEnum } from '@/utils/enum/modules/wms'

import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import FactorySelect from '@/components-system/base/factory-select.vue'
import WarehouseSelect from '@/components-system/wms/warehouse-select.vue'
import SteelPlate from './module/steel-plate.vue'
import SectionSteel from './module/section-steel.vue'
import SteelCoil from './module/steel-coil.vue'
import AuxMat from './module/aux-mat.vue'
import Gas from './module/gas.vue'
import RawMat from './module/raw-mat.vue'

const props = defineProps({
  basicClass: {
    type: Number
  },
  query: {
    type: Object,
    default: () => {
      return {}
    }
  },
  toQuery: {
    type: Function
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return SteelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return SectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return SteelCoil
    case rawMatClsEnum.MATERIAL.V:
      return AuxMat
    case rawMatClsEnum.GAS.V:
      return Gas
    default:
      return RawMat
  }
})

const queryVO = ref({})
watchEffect(() => {
  queryVO.value = props.query
})

// 查询
function toQuery() {
  if (typeof props.toQuery === 'function') {
    // 规格去空格后再返回
    if (queryVO.value.spec) {
      const specArr = queryVO.value.spec.split('*')
      queryVO.value.specification = specArr.map((s) => s.trim()).join('*')
    }
    props.toQuery()
  }
}
</script>
