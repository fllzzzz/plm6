<!-- 申购单/采购申请单:下拉选择框 -->
<template>
  <common-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    :options="options"
    :data-structure="DS"
    @change="handleChange"
  >
    <template #view="{ data }">
      <span class="option-item">
        <span>{{ data.serialNumber }}</span>
        <span>
          <span class="label">类型：</span><span v-parse-enum="{e:rawMatClsEnum, v:data.basicClass, bit:true, split: ' | '}"></span>
        </span>
      </span>
      <!-- <common-button icon="el-icon-view" type="info" size="mini" @click.stop="showPurchaseNoDetail(data)" /> -->
    </template>
  </common-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank, isBlank, judgeSameValue } from '@data-type/index'
import useUnclosedRequisition from '@compos/store/use-unclosed-requisition'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String]
  },
  projectId: {
    // 项目id
    type: [Array, Number]
  },
  publicWarehouse: {
    type: Boolean,
    default: false
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  viewsDetail: {
    // 可查看详情
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
  disabled: {
    type: Boolean,
    default: false
  },
  collapseTags: {
    type: Boolean,
    default: false
  },
  default: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '选择申购单'
  }
})

const DS = computed(() => {
  return {
    value: 'serialNumber',
    label: 'serialNumber',
    key: 'serialNumber'
  }
})

const selectValue = ref()

const { loaded, requisitions } = useUnclosedRequisition()

const options = computed(() => {
  let list = requisitions.value
  if (props.publicWarehouse) {
    list = requisitions.value.filter((v) => v.projectId === undefined)
  } else if (props.projectId) {
    if (Array.isArray(props.projectId)) {
      list = requisitions.value.filter((v) => props.projectId.includes(v.projectId))
    } else {
      list = requisitions.value.filter((v) => props.projectId === v.projectId)
    }
  }
  if (props.basicClass) {
    list = list.filter((v) => v.basicClass & props.basicClass)
  }
  return list
})

watch(options, (opt) => {
  loadedCallBack()
})

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
    // 有默认值的情况，并且value为空，则给value赋值
    if (props.default && isBlank(value) && isNotBlank(options.value)) {
      selectValue.value = options.value[0].value
      handleChange(selectValue.value)
    }
  },
  { immediate: true }
)

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

function loadedCallBack() {
  if (isNotBlank(options.value) && props.default && !selectValue.value) {
    selectValue.value = options.value[0].value
    handleChange(selectValue.value)
  }
}
</script>

<style lang="scss" scoped>
.option-item {
  width: 100%;
  display: inline-flex;
  justify-content: space-between;
}

.option-item > span:nth-child(1) {
  flex: none;
  margin-right: 15px;
}
.option-item > span:nth-child(2) {
  // flex: auto;
   color: #8492a6;
   font-size: 13px;
   .label {
     color: #9b6161
   }
}
</style>
