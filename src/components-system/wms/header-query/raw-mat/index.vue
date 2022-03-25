<template>
  <div class="first-line flex-rbc">
    <span class="child-mr-6">
      <slot name="firstItem" />
      <template v-if="showProjectWarehouseType">
        <common-radio-button
          v-model="queryVO.projectWarehouseType"
          :options="projectWarehouseTypeEnum.ENUM"
          default
          type="enum"
          size="small"
          class="filter-item"
          @change="handleProjectWarehouseTypeChange"
        />
        <project-cascader
          v-if="queryVO.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V"
          v-model="queryVO.projectId"
          placeholder="所属项目"
          clearable
          @change="toQuery"
          class="filter-item"
          style="width: 300px"
        />
      </template>
      <slot name="afterProjectWarehouseType" />
      <common-radio-button
        v-if="showMaterialIsWhole && [rawMatClsEnum.STEEL_PLATE.V, rawMatClsEnum.SECTION_STEEL.V].includes(basicClass)"
        v-model="queryVO.materialIsWhole"
        :options="materialIsWholeEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="toQuery"
      />
      <slot name="beforeWarehouse" />
      <factory-select v-model="queryVO.factoryId" placeholder="工厂" class="filter-item" @change="toQuery" clearable />
      <warehouse-select
        v-if="showWarehouse"
        v-model="queryVO.warehouseId"
        :factory-id="queryVO.factoryId"
        :basic-class="props.basicClass"
        placeholder="存储位置"
        class="filter-item"
        show-all
        clearable
        @change="toQuery"
      />
      <slot name="afterWarehouse" />
    </span>
    <span>
      <slot name="firstLineRight" />
    </span>
  </div>
  <slot name="secondLineFirstItem" class="child-mr-6" />
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
    placeholder="可选择/输入科目、编号搜索"
    @change="toQuery"
  />
  <component :is="comp" :query="query" :basic-class="props.basicClass" @to-query="toQuery" />
  <slot name="secondLineLastItem" />
</template>

<script setup>
import { defineEmits, defineProps, computed, watchEffect, ref } from 'vue'
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
const emit = defineEmits(['to-query'])

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
  },
  // 显示公共库/项目库
  showProjectWarehouseType: {
    type: Boolean,
    default: false
  },
  // 显示整料/余料
  showMaterialIsWhole: {
    type: Boolean,
    default: true
  },
  // 显示仓库
  showWarehouse: {
    type: Boolean,
    default: true
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
    props.toQuery()
  }
  emit('to-query')
}

function handleProjectWarehouseTypeChange(val) {
  if (val === projectWarehouseTypeEnum.PUBLIC.V) {
    queryVO.value.projectId = undefined
  }
  toQuery()
}
</script>
