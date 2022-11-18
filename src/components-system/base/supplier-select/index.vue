<!-- 供应商:下拉选择框 -->
<template>
  <span class="supplier-sn-select-container" :class="{ 'show-add-icon': props.logisticsCreateable }">
    <common-select
      v-model="selectValue"
      :size="props.size"
      :disabled="props.disabled"
      :multiple="props.multiple"
      :collapse-tags="props.collapseTags"
      :loading="!loaded"
      :clearable="props.clearable"
      :filterable="props.filterable"
      :placeholder="props.placeholder"
      :options="options"
      class="supplier-sn-select"
      @change="handleChange"
    >
      <template #view="{ data }">
        <span class="customize-option-item">
          <span class="flex-rsc label">
            <span>{{ data.name }}</span>
          </span>
          <span>
            <span v-if="props.showType" class="extra-label">
              <span class="title">类型：</span>
              <span v-parse-enum="{ e: supplierClassEnum, v: data.basicClass, bit: true, split: ' | ' }"></span>
            </span>
          </span>
        </span>
      </template>
    </common-select>
    <el-popover v-model:visible="addVisible" placement="top" width="450">
      <add-supplier @close="handlePopoverClose" />
      <template #reference>
        <span v-if="props.logisticsCreateable" class="add-icon pointer" @click.stop="addVisible = !addVisible">
          <el-icon v-permission="permission.add" color="#1881ef">
            <el-icon-plus />
          </el-icon>
        </span>
        <span v-else />
      </template>
    </el-popover>
  </span>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed, nextTick } from 'vue'
import { supplierIsHideEnum, supplierTypeEnum, supplierClassEnum } from '@/utils/enum/modules/supplier'
import { isBlank, isNotBlank, judgeSameValue } from '@data-type/index'
import useSuppliers from '@compos/store/use-suppliers'
import addSupplier from './module/add-supplier.vue'

const emit = defineEmits(['change', 'info-change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String]
  },
  basicClass: {
    // 供应商类型
    type: Number
  },
  logisticsCreateable: {
    // 可创建物流供应商
    type: Boolean,
    default: false
  },
  showType: {
    type: Boolean,
    default: true
  },
  type: {
    type: Number,
    default: supplierTypeEnum.RAW_MATERIAL.V | supplierTypeEnum.MANUFACTURED.V
  },
  mode: {
    // contained , contain, cross
    type: String,
    default: 'contained'
  },
  typeMode: {
    // contained , contain, cross
    type: String,
    default: 'cross'
  },
  showHide: {
    // 显示被列入黑名单的供应商
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
    default: true
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
    default: '请选择供应商'
  }
})

const permission = {
  add: ['wms_logisticsSupplier:add']
}

const addVisible = ref(false)
const selectValue = ref()

const { loaded, suppliers, supplierKV } = useSuppliers(loadedCallBack)

const options = computed(() => {
  const supplierList = props.showHide ? suppliers.value : suppliers.value.filter((v) => v.boolHide === supplierIsHideEnum.FALSE.V)
  if (props.basicClass) {
    if (props.mode === 'contained') {
      return supplierList.filter((v) => (v.basicClass & props.basicClass) === v.basicClass)
    }
    if (props.mode === 'contain') {
      return supplierList.filter((v) => (v.basicClass & props.basicClass) === props.basicClass)
    }
    if (props.mode === 'cross') {
      return supplierList.filter((v) => v.basicClass & props.basicClass)
    }
  }
  if (props.type) {
    if (props.typeMode === 'contained') {
      return supplierList.filter((v) => (v.type & props.type) === v.type)
    }
    if (props.typeMode === 'contain') {
      return supplierList.filter((v) => (v.type & props.type) === props.type)
    }
    if (props.typeMode === 'cross') {
      return supplierList.filter((v) => v.type & props.type)
    }
  }
  return supplierList
})

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
  },
  { immediate: true }
)

// watch(
//   [() => props.basicClass, () => props.type],
//   () => {
//     setDefault()
//   }
// )

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
    emitInfo(data, props.modelValue)
  }
}

function emitInfo(val, oldVal) {
  const res = val ? supplierKV.value[val] : null
  const oldRes = oldVal ? supplierKV.value[oldVal] : null
  emit('info-change', res, oldRes)
}

function loadedCallBack() {
  if (isNotBlank(selectValue.value)) {
    emitInfo(selectValue.value)
  } else {
    setDefault()
  }
}

function handlePopoverClose() {
  addVisible.value = false
}

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  nextTick(() => {
    if (isBlank(options.value) || selectValue.value) {
      return
    }
    if (props.default) {
      selectValue.value = options.value[0].id
      handleChange(selectValue.value)
      return
    }
  })
}
</script>

<style lang="scss" scoped>
.supplier-sn-select-container {
  display: inline-flex;
  position: relative;

  .supplier-sn-select {
    width: 100%;
  }
}

.show-add-icon {
  .add-icon {
    position: absolute;
    right: 5px;
    top: 50%;
    transform: translate(0, -50%);
    border: none;
    user-select: none;
    font-size: 14px;
    margin: 0 5px;
  }

  ::v-deep(.el-input__inner) {
    padding-right: 50px;
  }
  ::v-deep(.el-input__suffix) {
    right: 35px;
  }
}
</style>
