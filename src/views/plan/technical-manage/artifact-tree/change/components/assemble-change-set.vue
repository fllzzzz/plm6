<template>
  <common-table
    :header-cell-style="() => `font-weight: bold;color: #333333;`"
    :data="newAssembleList"
    style="max-width: 1100px"
    row-key="serialNumber"
    class="assemble-change-set-table"
    :expand-row-keys="expandRowKeys"
  >
    <el-expand-table-column :data="newAssembleList" v-model:expand-row-keys="expandRowKeys" row-key="serialNumber">
      <template #default="{ row }">
        <div class="set-con" v-if="row?.oldSerialNumbers?.length">
          <div v-for="item in row?.oldSerialNumbers" :key="item" class="set-item">
            <div>
              <div>
                <span>{{ item }} -> {{ row.serialNumber }}</span>
                <span></span>
              </div>
              <div>
                <span>差异</span>
              </div>
            </div>
            <div class="method-opt">
              <div v-for="item in assembleHandleMethodEnum.ENUM" :key="item.V" class="method-item">
                {{ item.L }}
              </div>
            </div>
          </div>
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100">
      <template #default="{ row }">
        <span style="font-weight: bold">{{ row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="120" />
    <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="100" />
    <el-table-column label="数量" prop="quantity" show-overflow-tooltip align="center" width="90" />
    <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" min-width="100" />
    <el-table-column label="操作" align="center" width="160">
      <template #default="{ row }">
        <common-radio
          v-model="row.operateType"
          :options="assembleOperateTypeEnum.ENUM"
          type="enum"
          @change="handleOperateTypeChange($event, row)"
        />
      </template>
    </el-table-column>
    <el-table-column label="旧部件编号" align="center" width="180">
      <template #default="{ row }">
        <common-select
          v-if="row.operateType !== assembleOperateTypeEnum.NEW.V"
          v-model="row.oldSerialNumbers"
          :options="oldAssembleList"
          type="other"
          placeholder="请选择"
          :data-structure="{ key: 'serialNumber', label: 'serialNumber', value: 'serialNumber' }"
          clearable
          collapseTags
          multiple
          style="width: 100%"
          @change="handleSerialNumberChange($event, row)"
        />
        <span v-else>/</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'

import { assembleOperateTypeEnum, assembleHandleMethodEnum } from './common'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'

defineProps({
  newAssembleList: {
    type: Array,
    default: () => []
  },
  oldAssembleList: {
    type: Array,
    default: () => []
  }
})

const expandRowKeys = ref([])

function handleOperateTypeChange(val, row) {
  if (val === assembleOperateTypeEnum.NEW.V) {
    row.oldSerialNumbers = []
  }
}

function handleSerialNumberChange(val, row) {
  if (val?.length && !expandRowKeys.value.includes(row.serialNumber)) {
    expandRowKeys.value.push(row.serialNumber)
  }
}
</script>

<style lang="scss" scoped>
.assemble-change-set-table {
  ::v-deep(.el-radio) {
    margin-right: 15px;
  }
}

.set-con {
  padding: 15px 35px;
  .set-item:not(:last-child) {
    margin-bottom: 10px;
  }

  .set-item {
    border: 1px dashed #797979;
    padding: 10px 20px;
    display: flex;
    align-items: center;
    justify-content: space-between;

    .method-opt {
      display: flex;

      .method-item {
        width: 50px;
        height: 29px;
        line-height: 29px;
        padding: 0px 6px 0px 8px;
        text-align: center;
        cursor: pointer;
        color: #fff;
        font-weight: bold;
        background-color: #cdcdcd;
        border-radius: 4px;
      }

      .method-item:not(:last-child) {
        margin-right: 8px;
      }
    }
  }
}
</style>
