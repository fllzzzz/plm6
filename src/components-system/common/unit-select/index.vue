<!-- 单位:下拉选择框 -->
<template>
  <el-select
    class="unit-select"
    v-model="copyValue"
    :size="size"
    :loading="!loaded"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :clearable="clearable"
    :filterable="filterable"
    :placeholder="placeholder"
    @change="selectChange"
    @blur="handleBlur"
  >
    <template v-if="!group">
      <template v-for="item in options">
        <el-option
          v-if="unshowOptions.indexOf(item.name) === -1"
          :key="item.id"
          :label="item.name"
          :value="item.name"
          :disabled="disabledName.includes(item.name)"
        >
          <span style="float: left">{{ item.name }}</span>
          <span v-if="showSymbol" style="float: right; font-size: 13px">{{ item.symbol }}</span>
        </el-option>
      </template>
    </template>
    <template v-else>
      <el-option-group v-for="group in groups" :key="group.name" :label="group.name">
        <template v-for="item in group.options">
          <el-option
            v-if="unshowOptions.indexOf(item.name) === -1"
            :key="item.id"
            :label="item.name"
            :value="item.name"
            :disabled="disabledName.includes(item.name)"
          >
            <span style="float: left">{{ item.name }}</span>
            <span v-if="showSymbol" style="float: right; font-size: 13px">{{ item.symbol }}</span>
          </el-option>
        </template>
      </el-option-group>
    </template>
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, computed, watch, ref } from 'vue'
import useUnit from '@compos/store/use-unit'

const emit = defineEmits(['change', 'blur', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String, Array]
  },
  options: {
    type: [Array, Object],
    default: () => []
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
  filterable: {
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
  unshowOptions: {
    type: Array,
    default: () => []
  },
  placeholder: {
    type: String,
    default: '单位'
  },
  disabledName: {
    type: Array,
    default: () => []
  },
  group: {
    type: Boolean,
    default: true
  },
  showSymbol: {
    type: Boolean,
    default: true
  },
  unitType: {
    type: String
  }
})

const copyValue = ref()
const { unit, loaded } = useUnit()

const options = computed(() => {
  if (props.unitType) {
    return unit.value[props.unitType] || []
  } else {
    return unit.value.ALL || []
  }
})

const groups = computed(() => {
  if (props.unitType) {
    for (const item of unit.value.GROUP) {
      if (item.type === props.unitType) return [item]
    }
    return []
  } else {
    return unit.value.GROUP || []
  }
})

watch(
  () => props.modelValue,
  (value) => {
    copyValue.value = value
  },
  { immediate: true }
)

function selectChange(val) {
  if (val === '') val = undefined
  emit('update:modelValue', val)
  emit('change', val)
}

function handleBlur(event) {
  emit('blur', event)
}
</script>
