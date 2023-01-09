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
    <template v-for="(group, type) in cutConfigs" :key="type">
      <el-option-group v-if="isBlank(layOffWayType) || Boolean(layOffWayTypeEnum.V[type].V) === Boolean(layOffWayType)" :label="layOffWayTypeEnum.VL[type]">
        <el-option
          v-for="item in group"
          :key="item.id"
          :label="item.name"
          :value="item.id"
          :disabled="disabledValue ? disabledValue.indexOf(item.id) > -1 : false"
        />
      </el-option-group>
    </template>
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import useCutConfig from '@compos/store/use-cut-config'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  layOffWayType: {
    type: [Boolean, undefined],
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

const { loaded, cutConfigs } = useCutConfig()

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
