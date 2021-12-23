<template>
  <div class="head-container">
    <material-info class="filter-item" :basic-class="basicClass" :material="currentSource" />
    <div class="filter-container">
      <div class="filter-left-box">
        <span class="total-info">
          <span class="info-item">
            <span>总件数({{ baseUnit.measure.unit }})</span>
            <span v-to-fixed="{ val: allQuantity || 0, dp: baseUnit.measure.precision }" />
          </span>
          <span class="info-item">
            <span>总重量({{ baseUnit.weight.unit }})</span>
            <span v-to-fixed="{ val: allMete || 0, dp: baseUnit.weight.precision }" />
          </span>
        </span>
      </div>
      <div class="filter-right-box child-mr-7">
        <store-operation v-if="!props.edit" type="cu" @clear="handleClear" />
        <common-button :loading="cu.status.edit === FORM.STATUS.PROCESSING" size="mini" type="primary" @click="cu.submit">
          提 交
        </common-button>
        <common-button type="success" @click="openReturnableList" size="mini">检索退库材料</common-button>
      </div>
    </div>
  </div>
  <returnable-list-drawer v-model="returnableVisible" :basic-class="basicClass" :select-list="form.list" @add="handleAdd" />
</template>

<script setup>
import { ref, defineEmits, defineProps, defineExpose } from 'vue'

import { regExtra } from '@/composables/form/use-form'
import MaterialInfo from '@/views/wms/return-application/components/material-info/index.vue'
import ReturnableListDrawer from '@/views/wms/return-application/components/returnable-list-drawer/index.vue'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import { toFixed } from '@/utils/data-type'
import StoreOperation from '@crud/STORE.operation.vue'

const emit = defineEmits(['add'])

const { cu, form, FORM } = regExtra() // 表单

const props = defineProps({
  currentSource: {
    type: Object,
    default: () => {
      return {}
    }
  },
  basicClass: {
    type: Number
  },
  list: {
    type: Array,
    default: () => []
  }
})

// 总重量
const allMete = ref()
// 总数量
const allQuantity = ref()
// 显示可归还列表
const returnableVisible = ref(false)
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(props.basicClass)

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  init()
}

// 初始化
function init() {
  allMete.value = 0
  allQuantity.value = 0
}

// 添加
function handleAdd(data) {
  emit('add', data)
}

// 打开
function openReturnableList() {
  returnableVisible.value = true
}

// 计算所有退库钢材总重
function calcAllWeight() {
  allMete.value = form.list.reduce((sum, { mete = 0 }) => {
    return +toFixed(sum + mete, baseUnit.value.weight.precision)
  }, 0)
}

// 计算所有退库钢材总数量
function calcAllQuantity() {
  allQuantity.value = form.list.reduce((sum, { quantity = 0 }) => {
    return +toFixed(sum + quantity, baseUnit.value.measure.precision)
  }, 0)
}

// 清除
function handleClear() {}

defineExpose({
  calcAllWeight,
  calcAllQuantity
})
</script>

<style lang="scss" scoped>
.filter-left-box {
  width: 100%;
}

.total-info {
  line-height: 15px;
  .info-item {
    display: inline-block;
    margin: 5px 0;
    font-size: 13px;
    min-width: 180px;
    > span {
      display: inline-block;
      overflow: hidden;
    }
    > span:first-child {
      font-weight: bold;
      width: 80px;
      text-align: right;
      &:after {
        content: '：';
      }
    }
  }
}
</style>
