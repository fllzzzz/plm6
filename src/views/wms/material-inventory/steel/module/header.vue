<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery">
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.basicClass"
            :options="steelClsEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
        </template>
      </mat-header-query>
      <rr-operation />
    </div>
    <crud-operation>
      <!-- TODO:打印 -->
      <template #optLeft>
        <common-button class="filter-item" v-permission="permission.outbound" type="primary" size="mini" @click="toBatchOutbound">
         <svg-icon icon-class="wms-outbound" /> 批量出库
        </common-button>
        <common-button class="filter-item" v-permission="permission.transfer" type="warning" size="mini" @click="toBatchTransfer">
          <svg-icon icon-class="wms-transfer" /> 批量调拨
        </common-button>
      </template>
      <template #viewLeft>
        <common-button v-permission="permission.get"  class="filter-item" type="info" size="mini" icon="el-icon-lock" @click="openFreezeRecords">
          冻结记录
        </common-button>
        <common-button class="filter-item" type="success" size="mini" icon="el-icon-printer" @click="toBatchPrint">批量打印</common-button>
        <el-badge :value="notPrintedMaterialQuantity" :hidden="notPrintedMaterialQuantity <= 0">
          <common-button class="filter-item" type="primary" size="mini" icon="el-icon-printer" @click="toPrintNotPrintedLabel">新入库标签打印</common-button>
        </el-badge>
      </template>
    </crud-operation>
  </div>
</template>

<script setup>
import { inject, ref } from 'vue'
import { steelClsEnum } from '@/utils/enum/modules/classification'
import { regHeader } from '@compos/use-crud'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'

const permission = inject('permission')

// 查询参数
const defaultQuery = {
  basicClass: { value: steelClsEnum.STEEL_PLATE.V, resetAble: false }
}

const { CRUD, crud, query } = regHeader(defaultQuery)

const notPrintedMaterialQuantity = ref(0)

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  notPrintedMaterialQuantity.value = data.notPrintedMaterialQuantity || 0
}

// 基础类型发生变化
async function handleBasicClassChange() {
  await crud.resetQuery()
  crud.data = []
}

// 批量出库
function toBatchOutbound() {}

// 批量调拨
function toBatchTransfer() {}

// 批量打印
function toBatchPrint() {}

// 打印 未打印标签的物料
function toPrintNotPrintedLabel() {

}

// 打开冻结记录
function openFreezeRecords() {

}
</script>
