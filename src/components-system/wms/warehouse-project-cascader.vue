<template>
  <common-radio-button
    v-bind="$attrs"
    v-model="currentProjectWarehouseType"
    :options="projectWarehouseTypeEnum.ENUM"
    :show-option-all="showOptionAll"
    type="enum"
    size="small"
    @change="handleTypeChange"
  />
  <project-cascader
    v-bind="$attrs"
    v-model="currentProjectId"
    :disabled="currentProjectWarehouseType === projectWarehouseTypeEnum.PUBLIC.V"
    placeholder="所属项目"
    clearable
    style="width: 250px"
    @change="handleProjectChange"
  />
</template>

<script setup>
import { ref, watch, defineEmits, defineProps } from 'vue'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

const emits = defineEmits(['change', 'update:projectWarehouseType', 'update:projectId'])

const currentProjectWarehouseType = ref()
const currentProjectId = ref()

const props = defineProps({
  projectWarehouseType: {
    type: Number
  },
  projectId: {
    type: [Number, String]
  },
  showOptionAll: {
    type: Boolean,
    default: true
  }
})

watch(
  () => props.projectWarehouseType,
  (val) => {
    currentProjectWarehouseType.value = val
  },
  { immediate: true }
)

watch(
  () => props.projectId,
  (val) => {
    currentProjectId.value = val
  },
  { immediate: true }
)

function handleTypeChange(val) {
  if (val === projectWarehouseTypeEnum.PUBLIC.V) {
    handleProjectChange(undefined)
  }
  emits('update:projectWarehouseType', val)
  emits('change')
}

function handleProjectChange(val) {
  emits('update:projectId', val)
  emits('change')
}
</script>
