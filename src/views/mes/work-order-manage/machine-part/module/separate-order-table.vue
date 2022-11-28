<template>
  <table class="separate-table" cellspacing="0" cellpadding="0" border="1">
    <tbody>
      <template v-for="(item, index) in separateOrderInfo" :key="index">
        <tr class="separate-thead">
          <td style="width: 120px">{{ item.serialNumber }}</td>
          <td v-for="(line, lineIndex) in item.productionLineList" :key="lineIndex">
            <span v-if="isNotBlank(line)">{{ line.productionLineName }}>{{ line.groupName }}</span>
            <span v-else>\</span>
          </td>
        </tr>
        <tr>
          <td class="separate-td-img">
            <img v-if="item.picturePath" :src="item.picturePath" alt="加载失败" />
            <span v-else>\</span>
          </td>
          <td v-for="(line, lineIndex) in item.productionLineList" :key="lineIndex">
            <span v-if="isNotBlank(line)">{{ line.quantity }}</span>
            <span v-else>\</span>
          </td>
        </tr>
      </template>
    </tbody>
  </table>
</template>

<script setup>
import { defineProps } from 'vue'
import { isNotBlank } from '@data-type/index'

defineProps({
  separateOrderInfo: {
    type: Array,
    default: () => []
  }
})
</script>

<style lang="scss" scoped>

.separate-table {
  width: 100%;
  table-layout: fixed;
}

.separate-table td {
  text-align: center;
}
.separate-table .separate-thead {
  background-color: #d9d9d9;
}
.separate-table .separate-thead td {
  font-size: 12px;
}
.separate-table .separate-td-img {
  height: 80px;
}

.separate-table .separate-td-img img {
  width: 95%;
}
</style>
