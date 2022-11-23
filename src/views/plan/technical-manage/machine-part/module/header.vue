<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        :productType="TechnologyTypeAllEnum.STRUCTURE.V"
        :default="false"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getAreaInfo="getAreaInfo"
      />
      <common-select
        v-model="query.areaId"
        :options="areaInfo"
        type="other"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;margin-left:3px;"
        @change="crud.toQuery"
      />
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import monomerSelect from '@/components-system/plan/monomer-select'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

const defaultQuery = {
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const areaInfo = ref([])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

function getAreaInfo(val) {
  areaInfo.value = val || []
}
</script>
