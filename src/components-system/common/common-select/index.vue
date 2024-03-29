<!-- 通用:下拉选择框 -->
<template>
  <el-select
    :key="`common_select_${key}`"
    v-model="selectValue"
    :size="props.size"
    :disabled="props.disabled"
    :multiple="props.multiple"
    :collapse-tags="props.collapseTags"
    :loading="loading"
    :clearable="props.clearable"
    :filterable="props.filterable"
    :placeholder="props.placeholder"
    :no-data-text="props.noDataText"
    :class="textAlignClass"
    @change="handleChange"
    @blur="handleBlur"
  >
    <el-option v-if="props.showOptionAll" :key="-1" :label="props.allLabelText" :value="allVal" :disabled="disabledVal.includes(allVal)" />
    <el-option
      v-if="showExtra"
      :key="-2"
      :label="extraOptionLabel"
      :value="extraOptionValue"
      :disabled="disabledVal.includes(extraOptionValue)"
    />
    <template v-for="item in options">
      <el-option
        v-if="unshowOptions.indexOf(item[DS.key]) === -1"
        :key="item[DS.key]"
        :label="item[DS.label]"
        :value="item[DS.value]"
        :disabled="disabledVal.includes(item[DS.value])"
      >
        <slot name="view" :data="item" />
      </el-option>
    </template>
    <template #empty v-if="slotsEmpty">
      <slot name="empty" />
    </template>
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, computed, useSlots, watch, ref } from 'vue'
import { getBits } from '@data-type/number'
import { deepClone, isBlank, isNotBlank, judgeSameValue } from '@data-type/index'
import { obj2arr } from '@/utils/convert/type'

import useCommonDataStructureByType from '@compos/use-common-data-structure-by-type'

const emit = defineEmits(['change', 'blur', 'update:modelValue'])
const slots = useSlots()
const slotsEmpty = ref(!!slots.empty)
const props = defineProps({
  modelValue: {
    type: [Number, String, Array, Boolean]
  },
  options: {
    type: [Array, Object],
    default: () => []
  },
  loading: {
    type: Boolean,
    default: false
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
  showOptionAll: {
    type: Boolean,
    default: false
  },
  unshowOptions: {
    type: Array,
    default: () => []
  },
  showExtra: {
    type: Boolean,
    default: false
  },
  extraOptionLabel: {
    type: String,
    default: '同上'
  },
  extraOptionValue: {
    type: [Number, String, Array, Boolean],
    default: -1
  },
  placeholder: {
    type: String,
    default: '请选择'
  },
  noDataText: {
    type: String
  },
  textAlign: {
    type: String,
    default: 'left'
  },
  allLabelText: {
    type: String,
    default: '全部'
  },
  allVal: {
    type: [Number, String],
    default: undefined
  },
  disabledVal: {
    type: Array,
    default: () => []
  },
  type: {
    // dict , enum, other
    type: String,
    default: 'other'
  },
  mode: {
    // normal , bit
    type: String,
    default: 'normal'
  },
  default: {
    type: Boolean,
    default: false
  },
  onlyOneDefault: {
    type: Boolean,
    default: false
  },
  dataStructure: {
    // 数据结构， type不选择dict与enum的情景下，可使用
    type: Object
  }
})

const selectValue = ref(props.modelValue)
const key = ref(Math.random())
// 是否已触发过默认选中
const hasDefaultTriggered = ref(false)

const allVal = computed(() => {
  if (isBlank(props.allVal) && props.mode === 'bit') {
    if (Array.isArray(props.options)) {
      return props.options.reduce((res, cur) => {
        return res | cur
      }, 0)
    }
  }
  return props.allVal
})

// 数据结构
const DS = useCommonDataStructureByType(props.type, props.dataStructure)

// text文字水平对齐方式
const textAlignClass = computed(() => {
  switch (props.textAlign) {
    case 'center':
      return 'alignCenter'
    case 'left':
      return 'alignLeft'
    case 'right':
      return 'alignRight'
    default:
      return 'alignLeft'
  }
})

// 监听modelValue，为selectValue赋值
watch(
  () => props.modelValue,
  (value) => {
    if (props.multiple && props.mode === 'bit') {
      selectValue.value = getBits(props.options, value, 'value', DS)
    } else {
      selectValue.value = value
    }
    setDefault()
  },
  { immediate: true }
)

// 监听options 与 是否显示额外选项。若值不存在，则设置值为空
watch(
  [() => props.options, () => props.showExtra],
  ([nVal], [oVal]) => {
    if (!props.loading) {
      let options = nVal ? deepClone(nVal) : []
      if (!Array.isArray(options)) {
        options = obj2arr(options)
      }
      if (props.showOptionAll) {
        const aOpt = {}
        aOpt[DS.value] = allVal.value
        options.push(aOpt)
      }
      if (props.showExtra && props.extraOptionLabel) {
        const eOpt = {}
        eOpt[DS.value] = props.extraOptionValue
        options.push(eOpt)
      }
      let cv = selectValue.value
      if (Array.isArray(selectValue.value)) {
        cv = options.filter((v) => selectValue.value.includes(v[DS.value])).map((v) => v[DS.value])
      } else {
        const isExit = options.some((v) => v[DS.value] === selectValue.value)
        if (!isExit) {
          cv = undefined
        }
      }
      selectValue.value = cv
      if (isBlank(cv)) {
        setDefault()
      } else {
        handleChange(cv)
      }
    }
    key.value = Math.random()
  },
  { immediate: true }
)

// 处理值发生改变的情况
function handleChange(val) {
  let data = val
  if (isBlank(val)) {
    data = undefined
  } else {
    if (props.multiple && props.mode === 'bit') {
      data = val.reduce((res, cur) => {
        return res | cur
      }, 0)
    }
  }
  // 发生变化
  const isChange = !judgeSameValue(data, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(data) && isBlank(props.modelValue)
  if (isChange && !allBlank) {
    emit('update:modelValue', data)
    emit('change', data)
  }
}

// 处理光标离开输入框
function handleBlur(event) {
  emit('blur', event)
}

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  // 有值不处理
  if (isNotBlank(selectValue.value)) {
    return
  }
  // 无值返回undefined
  if (isBlank(props.options) || hasDefaultTriggered.value) {
    handleChange(undefined)
    return
  }
  // 只有一个时默认选中
  if (props.onlyOneDefault && props.options.length === 1) {
    hasDefaultTriggered.value = true
    selectValue.value = props.options[0][DS.value]
    handleChange(selectValue.value)
    return
  }
  // 默认选中
  if (props.default) {
    hasDefaultTriggered.value = true
    selectValue.value = props.multiple ? [props.options[0][DS.value]] : props.options[0][DS.value]
    handleChange(selectValue.value)
    return
  }
  handleChange(undefined)
}
</script>

<style lang="scss" scoped>
.alignCenter {
  ::v-deep(.el-input input) {
    text-align: center;
  }
}
.alignLeft {
  ::v-deep(.el-input input) {
    text-align: left;
  }
}
.alignRight {
  ::v-deep(.el-input input) {
    text-align: right;
  }
}
</style>
