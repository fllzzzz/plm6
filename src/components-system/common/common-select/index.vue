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
    :class="textAlignClass"
    @change="handleChange"
    @blur="handleBlur"
  >
    <el-option v-if="props.showOptionAll" :key="-1" :label="props.allLabelText" :value="allVal" :disabled="disabledVal.includes(allVal)" />
    <el-option
      v-if="showExtra"
      :key="-2"
      :label="extraOption.label"
      :value="extraOption.value"
      :disabled="disabledVal.includes(extraOption.value)"
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
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, computed, watch, ref } from 'vue'
import { getBits } from '@data-type/number'
import { deepClone, isBlank, judgeSameValue } from '@data-type/index'
import { obj2arr } from '@/utils/convert/type'

import useCommonDataStructureByType from '@compos/use-common-data-structure-by-type'

const emit = defineEmits(['change', 'blur', 'update:modelValue'])

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
  extraOption: {
    type: Object,
    default: () => {
      return {
        label: '同上',
        value: -1
      }
    }
  },
  placeholder: {
    type: String,
    default: '请选择'
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
      if (props.showExtra && props.extraOption) {
        const eOpt = {}
        eOpt[DS.value] = props.extraOption.value
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

function handleBlur(event) {
  emit('blur', event)
}

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  if (isBlank(props.options) || selectValue.value) {
    return
  }
  if (props.onlyOneDefault && props.options.length === 1) {
    selectValue.value = props.options[0].value
    handleChange(selectValue.value)
    return
  }
  if (props.default) {
    selectValue.value = props.options[0].value
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
