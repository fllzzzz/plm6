<template>
  <div class="head-container">
    <material-info class="filter-item" :basic-class="basicClass" :material="currentSource" />
    <div class="filter-container">
      <div class="filter-left-box">
        <span class="total-info">
          <template v-if="basicClass & STEEL_ENUM">
            <span class="info-item">
              <span>总数({{ baseUnit.measure.unit }})</span>
              <span v-to-fixed="{ val: allQuantity || 0, dp: baseUnit.measure.precision }" />
            </span>
            <span class="info-item">
              <span>总重量({{ baseUnit.weight.unit }})</span>
              <span v-to-fixed="{ val: allMete || 0, dp: baseUnit.weight.precision }" />
            </span>
            <span v-if="basicClass === rawMatClsEnum.SECTION_STEEL.V" class="info-item">
              <span>总长度(m)</span>
              <span v-to-fixed="{ val: allLength || 0, dp: 3 }" />
            </span>
          </template>
        </span>
      </div>
      <div class="filter-right-box child-mr-7">
        <store-operation v-if="!props.edit" type="cu" @clear="handleClear" />
        <!-- <common-button v-if="cu.props.abnormalList" type="danger" @click="abnormalVisible = true" size="mini">异常列表</common-button> -->
        <common-button :loading="cu.status.edit === FORM.STATUS.PROCESSING" size="mini" type="primary" @click="cu.submit">
          提 交
        </common-button>
        <common-button type="success" @click="openReturnableList" size="mini">检索可退库材料</common-button>
        <common-button v-if="!props.edit" icon="el-icon-time" type="info" size="mini" @click="toReturnRecord" />
      </div>
    </div>
  </div>
  <returnable-list-drawer
    ref="returnableListRef"
    v-model="returnableVisible"
    :basic-class="basicClass"
    :select-list="form.list"
    :source-return-ids="sourceReturnIds"
    @add="handleAdd"
    :edit="props.edit"
  />
  <!-- <common-dialog v-model="abnormalVisible" title="异常" :show-close="true" width="90%">
    <abnormal-list :basicClass="basicClass" :list="cu.props.abnormalList" :maxHeight="700" />
  </common-dialog> -->
</template>

<script setup>
import { ref, defineEmits, defineProps, defineExpose, nextTick } from 'vue'
import { useRouter } from 'vue-router'

import { toFixed } from '@/utils/data-type'
import { STEEL_ENUM } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regExtra } from '@/composables/form/use-form'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

import MaterialInfo from '@/views/wms/material-return/raw-material/application/components/material-info/index.vue'
import ReturnableListDrawer from '@/views/wms/material-return/raw-material/application/components/returnable-list-drawer/index.vue'
import StoreOperation from '@crud/STORE.operation.vue'
// import AbnormalList from '../components/abnormal-list'

const emit = defineEmits(['add'])
const { cu, form, FORM } = regExtra() // 表单

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  sourceReturnIds: {
    type: Array,
    default: () => []
  },
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

const router = useRouter()
// 可退库列表
const returnableListRef = ref()
// 总重量
const allMete = ref()
// 总数量
const allQuantity = ref()
// 总长度
const allLength = ref()

// 显示可归还列表
const returnableVisible = ref(false)
// 显示异常列表
// const abnormalVisible = ref(false)
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(props.basicClass)

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  if (!props.edit) returnableListRef.value && returnableListRef.value.refresh()
  init()
}

// 初始化
function init() {
  allMete.value = 0
  allQuantity.value = 0
  allLength.value = 0
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
  nextTick(() => {
    allMete.value = form.list.reduce((sum, cur) => {
      return +toFixed(sum + (cur.mete || 0), baseUnit.value.weight.precision)
    }, 0)
  })
}

// 计算所有退库钢材总数量
function calcAllQuantity() {
  allQuantity.value = form.list.reduce((sum, { quantity = 0 }) => {
    return +toFixed(sum + quantity, baseUnit.value.measure.precision)
  }, 0)
}

// 计算所有型材总长
function calcAllLength() {
  allLength.value = form.list.reduce((sum, cur) => {
    return +toFixed(sum + (cur.totalLength || 0), 2)
  }, 0)
}

// 前往退库记录
function toReturnRecord() {
  router.push({ name: 'RawMatReturnApplicationRecord', params: { basicClass: props.basicClass }})
}

// 清除
function handleClear() {}

defineExpose({
  calcAllWeight,
  calcAllQuantity,
  calcAllLength
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
      width: 90px;
      text-align: right;
      &:after {
        content: '：';
      }
    }
  }
}
</style>
