<template>
  <common-table class="list-table" v-bind="$attrs" ref="tableRef" :data="list" :data-format="columnsDataFormat" :height="props.height" row-key="id">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column label="钢材种类" key="steelClassifyConfName" prop="steelClassifyConfName" align="center" width="100" />
    <el-table-column label="材质" key="material" prop="material" align="center" width="100" />
    <el-table-column label="厚度/规格" key="specification" prop="specification" align="center" />
    <el-table-column label="清单量（kg）" key="listMete" prop="listMete" align="center" width="110">
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
    <el-table-column label="备料量（kg）" key="preparationMete" prop="preparationMete" align="center" width="110">
      <template #default="{ row: { sourceRow: row } }">
        <span v-if="techPrepMeteKV[row.id]" v-to-fixed="{ val: techPrepMeteKV[row.id].preparation, k: 'COM_WT__KG' }" />
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column label="差值（kg）" key="diff" prop="diff" align="center" width="100">
      <template #default="{ row: { sourceRow: row } }">
        <span
          v-if="techPrepMeteKV[row.id]"
          :class="techPrepMeteKV[row.id].isEnough ? 'over-text' : 'not-over-text'"
          v-to-fixed="{ val: techPrepMeteKV[row.id].diff, k: 'COM_WT__KG' }"
          v-prefix="techPrepMeteKV[row.id].isEnough && techPrepMeteKV[row.id].diff !== 0 ? '+' : ''"
        />
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column v-if="props.editMode" label="操作" key="preparationMete" prop="preparationMete" align="center" width="50">
      <template #default="{ row: { sourceRow: row }, $index }">
        <!-- 当有绑定的库存利用清单或需要采购清单时，无法被删除 -->
        <el-tooltip
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
        </el-tooltip>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { STEEL_BASE_UNIT } from '@/settings/config'
import { toPrecision } from '@/utils/data-type'
import { operationTypeEnum } from '@/utils/enum/modules/common'
import { ref, defineProps, nextTick } from 'vue'

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

// 表格ref
const tableRef = ref()
// 列格式转换
const columnsDataFormat = [['listMete', ['to-fixed', STEEL_BASE_UNIT.weight.precision]]]
// 技术清单汇总列表
// const list = ref([])

// 删除
function removeRow(index) {
  const list = props.list
  list.splice(index, 1)
}

// 处理清单量变化
function handleListMeteChange(newVal, oldVal, row) {
  const triggerCalc = () => {
    if (oldVal !== row.listMete) {
      if (row.operateType !== operationTypeEnum.ADD.V) {
        row.operateType = operationTypeEnum.EDIT.V
      }
      // 重新计算差值
      const info = props.techPrepMeteKV[row.id]
      info.diff = toPrecision(info.preparation - row.listMete, STEEL_BASE_UNIT.weight.precision) // 差值 = 总备料量 - 清单量
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
