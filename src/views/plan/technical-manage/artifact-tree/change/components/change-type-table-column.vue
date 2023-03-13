<template>
  <el-table-column key="invoiceType" prop="invoiceType" label="类型" align="center" width="70px">
    <template #default="{ row }">
      <div v-if="row.changeType" class="change-type-con" :style="{ backgroundColor: changeTypeEnum.V[row.changeType].C }">
        {{ changeTypeEnum.VL[row.changeType] }}
      </div>
    </template>
  </el-table-column>
</template>

<script setup>
import { changeTypeEnum } from './common.js'
</script>

<style lang="scss" scoped>
.change-type-con {
  width: 30px;
  height: 30px;
  line-height: 30px;
  text-align: center;
  color: #fff;
  font-weight: bold;
  border-radius: 50%;
  margin: auto;
}
</style>
