<!-- 采购订单:下拉选择框 -->
<template>
  <span class="purchase-sn-select-container" :class="{ 'show-detail-icon': props.detailable }">
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
      class="purchase-sn-select"
      @change="handleChange"
    >
      <template #view="{ data }">
        <span class="customize-option-item">
          <span class="flex-rsc label">
            <el-tooltip content="点击可查看详情" placement="left" :show-after="1000">
              <el-icon
                v-if="props.detailable"
                v-permission="permission.get"
                @click.stop="openDetail(data.id)"
                class="pointer"
                color="#1881ef"
              >
                <el-icon-view />
              </el-icon>
            </el-tooltip>
            <span>{{ data.serialNumber }}</span>
          </span>
          <span>
            <span class="extra-label">
              <span class="title">类型：</span>
              <span v-parse-enum="{ e: rawMatClsEnum, v: data.basicClass, bit: true, split: ' | ' }"></span>
            </span>
            <span v-if="data.supplier" class="extra-label">
              <span class="title">供应商：</span>
              <span class="more-text-info ellipsis-text">{{ data.supplier.name }}</span>
            </span>
            <span v-if="data.projectNames" class="extra-label">
              <span class="title">项目：</span>
              <span class="more-text-info ellipsis-text">{{ data.projectNames }}</span>
            </span>
          </span>
        </span>
      </template>
    </common-select>
    <span
      v-if="props.detailable"
      class="detail-icon pointer"
      :class="{ 'not-allowed': !selectValue }"
      @click.stop="openDetail(selectValue)"
    >
      <el-icon v-permission="permission.get" :color="selectValue ? '#1881ef' : '#c1c2c5'">
        <el-icon-view />
      </el-icon>
    </span>
    <m-detail v-model="visible" :detail-id="currentId" />
  </span>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank, isBlank, judgeSameValue, deepClone } from '@data-type/index'
import useUnclosedPurchaseOrder from '@compos/store/use-unclosed-purchase-order'
import mDetail from './module/detail.vue'

const emit = defineEmits(['change', 'info-change', 'update:modelValue', 'update:info'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String]
  },
  info: {
    type: Object
  },
  detailable: {
    // 可查看详情
    type: Boolean,
    default: true
  },
  basicClass: {
    // 基础分类
    type: Number
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
  onlyOneDefault: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '选择采购订单'
  }
})

const permission = {
  get: ['wms_purchaseOrder:get']
}

const DS = computed(() => {
  return {
    value: 'id',
    label: 'serialNumber',
    key: 'id'
  }
})

const selectValue = ref()
const purchaseOrderKV = ref({})
const visible = ref(false)
const currentId = ref()

const { loaded, purchaseOrder } = useUnclosedPurchaseOrder(loadedCallBack)

const options = computed(() => {
  let list = deepClone(purchaseOrder.value)
  if (props.basicClass) {
    list = list.filter((v) => v.basicClass & props.basicClass)
    list = list.map((v) => {
      v.projectNames = v.projects ? v.projects.map((v) => v.shortName).join('、') : ''
      return v
    })
  }
  return list
})

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
    setDefault()
  },
  { immediate: true }
)

function openDetail(id) {
  if (!id) return
  visible.value = true
  currentId.value = id
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
    emitInfo(data)
  }
}

function emitInfo(val) {
  const res = val ? purchaseOrderKV.value[val] : null
  emit('update:info', res)
  emit('info-change', res)
}

function loadedCallBack() {
  purchaseOrderKV.value = {}
  if (isNotBlank(options.value)) {
    options.value.forEach((v) => {
      purchaseOrderKV.value[v.id] = v
    })
  }
  if (isNotBlank(selectValue.value)) {
    emitInfo(selectValue.value)
  } else {
    setDefault()
  }
}

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  if (isBlank(options.value) || selectValue.value) {
    return
  }
  if (props.onlyOneDefault && options.value.length === 1) {
    selectValue.value = options.value[0].value
    handleChange(selectValue.value)
    return
  }
  if (props.default) {
    selectValue.value = options.value[0].value
    handleChange(selectValue.value)
    return
  }
  // 未赋予默认值
  if (isBlank(selectValue.value) && isNotBlank(props.info)) {
    emitInfo()
  }
}
</script>

<style lang="scss" scoped>
.purchase-sn-select-container {
  display: inline-flex;
  position: relative;

  .purchase-sn-select {
    width: 100%;
  }
}

.show-detail-icon {
  .detail-icon {
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

.more-text-info {
  display: inline-block;
  line-height: 12px;
  max-width: 250px;
}
</style>
