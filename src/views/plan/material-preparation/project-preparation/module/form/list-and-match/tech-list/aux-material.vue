<template>
  <common-table
    class="list-table"
    v-bind="$attrs"
    ref="tableRef"
    :data="list"
    :data-format="columnsDataFormat"
    :height="props.height"
    row-key="id"
  >
    <!-- <el-table-column label="序号" type="index" align="center" width="60" /> -->
    <!-- 基础信息 -->
    <material-base-info-columns
      :show-serial-number="false"
      :basic-class="basicClass"
      show-frozen-tip
      frozen-viewable
      spec-merge
      sortable
      fixed="left"
    />
    <!-- 次要信息 -->
    <material-secondary-info-columns :basic-class="basicClass" />
    <!-- <el-table-column label="钢材种类" key="steelClassifyConfName" prop="steelClassifyConfName" align="center" width="100" />
    <el-table-column label="材质" key="material" prop="material" align="center" width="100" />
    <el-table-column label="厚度/规格" key="specification" prop="specification" align="center" /> -->
    <!-- 单位及其数量 -->
    <el-table-column key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" width="70px" show-overflow-tooltip />
    <el-table-column label="清单量" key="listMete" prop="listMete" align="center" width="110">
      <template #default="{ row }">
        <el-input-number
          v-if="props.editMode"
          v-model="row.sourceRow.listMete"
          :controls="false"
          :min="0"
          :max="999999999"
          placeholder="清单量"
          size="mini"
          @change="(nv, ov) => handleListMeteChange(nv, ov, row.sourceRow)"
        />
        <span v-else>{{ row.listMete }}</span>
      </template>
    </el-table-column>
    <el-table-column label="备料量" key="preparationMete" prop="preparationMete" align="center" width="110">
      <template #default="{ row: { sourceRow: row } }">
        <span v-if="techPrepMeteKV[row.id]" v-to-fixed="{ val: techPrepMeteKV[row.id].preparation, dp: row.accountingPrecision }" />
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column label="差值" key="diff" prop="diff" align="center" width="100">
      <template #default="{ row: { sourceRow: row } }">
        <span
          v-if="techPrepMeteKV[row.id]"
          :class="techPrepMeteKV[row.id].isEnough ? 'over-text' : 'not-over-text'"
          v-to-fixed="{ val: techPrepMeteKV[row.id].diff || 0, dp: row.accountingPrecision }"
          v-prefix="techPrepMeteKV[row.id].isEnough && techPrepMeteKV[row.id].diff !== 0 ? '+' : ''"
        />
        <span v-else>-</span>
      </template>
    </el-table-column>
    <!-- <el-table-column v-if="props.editMode" label="操作" key="preparationMete" prop="preparationMete" align="center" width="50">
      <template #default="{ row: { sourceRow: row }, $index }"> -->
    <!-- 当有绑定的库存利用清单或需要采购清单时，无法被删除 -->
    <!-- <el-tooltip
          class="item"
          effect="dark"
          content="当前清单有“绑定的库存利用清单”或“需要采购清单”，无法被删除"
          :disabled="!(row.boundInvIds && row.boundInvIds.length > 0)"
          placement="top-start"
        >
          <span>
            <common-button
              type="danger"
              icon="el-icon-delete"
              size="mini"
              style="padding: 6px"
              :disabled="row.boundInvIds && row.boundInvIds.length > 0"
              @click.stop="removeRow($index)"
            />
          </span>
        </el-tooltip> -->
    <!-- </template>
    </el-table-column> -->
  </common-table>
</template>

<script setup>
import { ref, defineProps, nextTick } from 'vue'
import { toPrecision } from '@/utils/data-type'
import { operationTypeEnum } from '@/utils/enum/modules/common'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
const props = defineProps({
  height: {
    type: Number,
    default: 250
  },
  list: {
    // 技术清单汇总列表
    type: Array,
    default: () => []
  },
  techPrepMeteKV: {
    type: Object,
    default: () => ({})
  },
  // 可操作的
  editMode: {
    type: Boolean,
    default: false
  }
})

const basicClass = matClsEnum.MATERIAL.V

// 表格ref
const tableRef = ref()
// 列格式转换
const columnsDataFormat = [['listMete', ['to-fixed-field', 'accountingPrecision']]]
// 技术清单汇总列表
// const list = ref([])

// 删除
// function removeRow(index) {
//   const list = props.list
//   list.splice(index, 1)
// }

// 处理清单量变化
function handleListMeteChange(newVal, oldVal, row) {
  const triggerCalc = () => {
    if (oldVal !== row.listMete) {
      if (row.operateType !== operationTypeEnum.ADD.V) {
        row.operateType = operationTypeEnum.EDIT.V
      }
      // 重新计算差值
      const info = props.techPrepMeteKV[row.id]
      info.diff = toPrecision(info.preparation - row.listMete, row.accountingPrecision) // 差值 = 总备料量 - 清单量
      info.isEnough = info.diff >= 0 // 是否超出
    }
  }

  if (!newVal || newVal <= 0) {
    // 设置为空 或 小于最小数量，则设置为最小数量
    nextTick(() => {
      row.listMete = oldVal
      triggerCalc()
    })
  } else {
    triggerCalc()
  }
}
</script>

<style scoped>
.list-table {
  width: 800px;
}
</style>
