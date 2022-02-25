<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :show-tips="areaInfo.length<=0"
        :productType="TechnologyTypeAllEnum.STRUCTURE.V"
        @getAreaInfo="getAreaInfo"
      />
      <area-tabs
        class="filter-item"
        :style="areaInfo.length>0?'width:calc(100% - 230px)':'width:calc(100% - 380px)'"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-radio-group v-model="query.status" size="small" class="filter-item" @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <el-radio-button v-for="item in processingEnum.ENUM" :key="item.V" :label="item.V">
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
      <common-radio-button
        v-model="query.shearType"
        :options="shearTypeEnum.ENUM"
        show-option-all
        type="enum"
        style="margin-left: 0; margin-right: 6px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.material"
        size="small"
        placeholder="输入材质搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import { processingEnum, shearTypeEnum } from '@enum-ms/plan'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { ElRadioGroup } from 'element-plus'

const defaultQuery = {
  serialNumber: '',
  specification: '',
  material: '',
  shearType: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  crud.toQuery()
}
function getAreaInfo(val) {
  areaInfo.value = val || []
  if (areaInfo.value.length > 0) {
    defaultTab.value = {
      id: areaInfo.value[0].id + '',
      name: areaInfo.value[0].name
    }
  } else {
    defaultTab.value = {}
  }
}
</script>
