<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-tabs v-model="query.type" class="tab-container" @tab-click="crud.toQuery">
        <el-tab-pane label="原材料" :name="String(supplierPayTypeEnum.MATERIAL.V)">
        </el-tab-pane>
        <el-tab-pane label="制成品" :name="String(supplierPayTypeEnum.PRODUCT.V)">
        </el-tab-pane>
        <el-tab-pane label="物流" :name="String(supplierPayTypeEnum.TRANSPORT.V)">
        </el-tab-pane>
        <!-- 后台暂时未提供字段 -->
        <!-- <el-tab-pane label="专业分包" :name="String(supplierPayTypeEnum.SUBCONTRACT.V)">
        </el-tab-pane> -->
      </el-tabs>
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 200px"
      />
      <el-radio-group v-model="query.settlementStatus" size="small" class="filter-item" @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <el-radio-button v-for="item in settlementStatusEnum.ENUM" :key="item.V" :label="item.V">
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
      <el-input v-model="query.supplierName" placeholder="供应商" style="width: 200px" class="filter-item" />
      <el-input v-model="query.orderSerialNumber" placeholder="订单号" style="width: 200px" class="filter-item" />
      <basic-class-select
        v-model="query.basicClass"
        clearable
        placeholder="物料种类"
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <rrOperation />
      <crudOperation>
        <!-- <template #viewLeft>
        </template> -->
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { settlementStatusEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import basicClassSelect from '@/components-system/classification/basic-class-select.vue'
import { ElRadioGroup, ElTabs, ElTabPane } from 'element-plus'

const defaultQuery = {
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  basicClass: undefined,
  orderSerialNumber: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  supplierName: undefined,
  type: String(supplierPayTypeEnum.MATERIAL.V)
}

const { CRUD, crud, query } = regHeader(defaultQuery)

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime && crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  }
}
</script>