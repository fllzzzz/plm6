<!-- 通用:下拉选择框 -->
<template>
  <el-select
    v-model="copyValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    :clearable="clearable"
    :filterable="filterable"
    :placeholder="placeholder"
    :class="textAlignClass"
    @change="selectChange"
    @blur="handleBlur"
  >
    <el-option
      v-if="showAll"
      :key="-1"
      :label="allLabelText"
      :value="allVal"
      :disabled="disabledVal.includes(allVal)"
    />
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
        <slot :data="item" />
      </el-option>
    </template>
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, computed, watch, ref } from 'vue'
import { isBlank } from '@data-type/index'
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
  showAll: {
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
  type: { // dict , enum, other
    type: String,
    default: 'dict'
  },
  dataStructure: {
    // 数据结构， type不选择dict与enum的情景下，可使用
    type: Object
  }
})

const loading = ref(false)
const copyValue = ref()

// 数据结构
const DS = useCommonDataStructureByType(props.type, props.dataStructure)

const textAlignClass = computed(() => {
  switch (props.textAlign) {
    case 'center': return 'alignCenter'
    case 'left': return 'alignLeft'
    case 'right': return 'alignRight'
    default: return 'alignLeft'
  }
})

watch(
  () => props.modelValue,
  (value) => { copyValue.value = value },
  { immediate: true }
)

function selectChange(val) {
  if (isBlank(val)) val = undefined
  emit('update:modelValue', val)
  emit('change', val)
}

function handleBlur(event) {
  emit('blur', event)
}
</script>

<style lang="scss" scoped>
.alignCenter {
  ::v-deep(.el-input input){
    text-align: center;
  }
}
.alignLeft {
  ::v-deep(.el-input input){
    text-align: left;
  }
}
.alignRight {
  ::v-deep(.el-input input){
    text-align: right;
  }
}
</style>
