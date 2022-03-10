<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getCurrentInfo="handleCurrent"
      />
      <el-input
        v-model="query.classifyName"
        size="small"
        placeholder="名称搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, watch, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  classifyName: '',
  serialNumber: '',
  monomerId: undefined,
  projectId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const emit = defineEmits(['currentChange'])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function handleCurrent(val) {
  emit('currentChange', val)
}
</script>
