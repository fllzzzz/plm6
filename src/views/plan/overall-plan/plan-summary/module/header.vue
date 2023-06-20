<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        placeholder="请选择"
        format="YYYY"
        value-format="YYYY"
        class="filter-item"
        @change="crud.toQuery"
        style="width:120px;"
      />
      <common-radio-button
        v-model="query.type"
        :options="typeOptions"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <el-tag>{{ query.type===TechnologyTypeAllEnum.STRUCTURE.V || query.type===TechnologyTypeAllEnum.BRIDGE.V? '单位(t)': '单位(m)' }}</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { mapGetters } from '@/store/lib'
import { TechnologyTypeAllEnum, projectTypeEnum } from '@enum-ms/contract'

const { globalProject } = mapGetters(['globalProject'])

const defaultQuery = {
  type: globalProject.projectType === projectTypeEnum.STEEL.V ? TechnologyTypeAllEnum.STRUCTURE.V : TechnologyTypeAllEnum.BRIDGE.V,
  year: String(new Date().getFullYear())
}

const { crud, query } = regHeader(defaultQuery)

const typeOptions = computed(() => {
  return globalProject.projectType === projectTypeEnum.STEEL.V ? [TechnologyTypeAllEnum.STRUCTURE] : [TechnologyTypeAllEnum.BRIDGE]
})
</script>
