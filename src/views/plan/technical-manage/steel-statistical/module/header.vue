<template>
  <div>
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optLeft>
          <common-select
            v-model="query.monomerIds"
            :options="monomerOption"
            :type="'other'"
            size="small"
            :dataStructure="typeProp"
            clearable
            multiple
            class="filter-item"
            placeholder="单体,可多选"
            style="width: 250px"
            @change="crud.toQuery"
          />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { monomerAll } from '@/api/plan/monomer'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
const defaultQuery = {
  monomerIds: undefined
}

const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const monomerOption = ref([])
watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      getMonomerAll(props.projectId)
      crud.toQuery()
    } else {
      monomerOption.value = []
    }
  },
  { immediate: true, deep: true }
)

async function getMonomerAll(id) {
  try {
    const { content } = await monomerAll(id)
    monomerOption.value = content
  } catch (e) {
    console.log('获取单体信息', e)
  }
}
</script>
