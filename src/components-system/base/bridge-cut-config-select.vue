<!-- 切割配置:下拉选择框 -->
<template>
  <el-select
    v-model="selectValue"
    :size="size"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    @change="handleChange"
  >
    <el-option-group v-for="(group, index) in bridgeCutConfigs" :key="index + 1" :label="layOffWayTypeEnum.VL[index]">
       <el-option
        v-for="item in group"
        :key="item.id"
        :label="item.name"
        :value="item.id"
        :disabled="isDisabled ? item.boolNestCutEnum : (disabledValue ? disabledValue.indexOf(item.id) > -1 : false)"
      />
    </el-option-group>
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import useBridgeCutConfig from '@compos/store/use-bridge-cut-config'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  layWayConfigId: {
    type: Number,
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  multiple: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  isDisabled: {
    type: Boolean,
    default: false
  },
  collapseTags: {
    type: Boolean,
    default: false
  },
  showOptionAll: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择切割配置'
  },
  disabledValue: {
    type: Array,
    default: () => []
  }
})

const selectValue = ref()

const { loaded, bridgeCutConfigs } = useBridgeCutConfig()

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
  },
  { immediate: true }
)

function handleChange(val) {
  console.log(val, 'val')
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
</script>
