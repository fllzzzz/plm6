<template>
  <div>
    <common-select
      v-model="selectValue"
      :options="subjectList"
      type="other"
      :placeholder="placeholder"
      clearable
      filterable
      :dataStructure="{ key: 'subjectId', label: 'name', value: 'subjectId' }"
      @change="handleChange"
    ></common-select>
  </div>
</template>

<script setup>
import { getMatClsByCls } from '@/api/config/classification-manage/common'
import { defineProps, defineEmits, defineExpose, watch, ref } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  basicClass: {
    type: Number,
    default: undefined
  },
  placeholder: {
    type: String,
    default: '请选择'
  }
})

const subjectList = ref([])
const selectValue = ref()

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
  },
  { immediate: true }
)

watch(
  () => props.basicClass,
  (val) => {
    if (val) {
      fetchMatCls()
    }
  },
  { immediate: true }
)

async function fetchMatCls() {
  try {
    const data = await getMatClsByCls(props.basicClass)
    subjectList.value = data
  } catch (error) {
    console.log(error, '根据基础物料分类查询科目')
  }
}

function handleChange(val) {
  let data = val
  if (isBlank(data)) data = undefined
  // 发生变化
  const isChange = !judgeSameValue(data, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(data) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', data)
    emit('change', data)
  }
}

function getOption(id) {
  return subjectList.value.find((v) => v.subjectId === id)
}

defineExpose({
  getOption
})
</script>

<style lang="scss" scoped></style>
