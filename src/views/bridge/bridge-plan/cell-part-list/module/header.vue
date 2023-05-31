<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :productType="TechnologyTypeAllEnum.BRIDGE.V"
        :show-tips="areaInfo.length <= 0"
        @getAreaInfo="getAreaInfo"
      />
      <area-tabs
        class="filter-item"
        :style="areaInfo.length > 0 ? 'width:calc(100% - 230px)' : 'width:calc(100% - 380px)'"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-input v-model="query.serialNumber" size="small" placeholder="输入编号搜索" style="width: 170px" class="filter-item" clearable />
      <el-input v-model="query.specification" size="small" placeholder="输入规格搜索" style="width: 170px" class="filter-item" clearable />
      <el-input v-model="query.material" size="small" placeholder="输入材质搜索" style="width: 170px" class="filter-item" clearable />
      <rrOperation />
    </div>
    <crudOperation>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: '',
  serialNumber: '',
  specification: '',
  material: '',
  areaId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
// const deleteLoading = ref(false)
// const errorList = ref([])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  globalProject: {
    type: Object,
    default: () => {}
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
  console.log(areaInfo.value)
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
