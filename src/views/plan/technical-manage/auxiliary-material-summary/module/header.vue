<template>
  <div>
    <crudOperation>
      <template #optLeft>
        <monomer-select
          ref="monomerSelectRef"
          v-model="query.monomerId"
          :project-id="props.projectId"
          class="filter-item"
          :default="false"
          clearable
          :productType="TechnologyTypeAllEnum.STRUCTURE.V"
          @getAreaInfo="getAreaInfo"
          @change="crud.toQuery"
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
          style="width:200px;"
          @change="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <print-table
          api-key="auxiliaryMaterialSummary"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

const defaultQuery = {
  monomerId: undefined,
  areaId: undefined
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
