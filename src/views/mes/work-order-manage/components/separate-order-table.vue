<template>
  <table class="separate-table" cellspacing="0" cellpadding="0" border="1">
    <tbody>
      <template v-for="(item, index) in separateOrderInfo" :key="index">
        <tr class="separate-thead">
          <td style="width: 120px">{{ item.serialNumber }}</td>
          <td v-for="(line, lineIndex) in item.productionLineList" :key="lineIndex">
            <div v-if="isNotBlank(line)">
              <span>{{ line.productionLineName }}>{{ line.groupName }}</span>
            </div>
            <span v-else>\</span>
          </td>
        </tr>
        <tr>
          <td class="separate-td-img">
            <img v-if="item.picturePath" :src="item.picturePath" alt="加载失败" />
            <span v-else>\</span>
          </td>
          <td v-for="(line, lineIndex) in item.productionLineList" :key="lineIndex">
            <div v-if="isNotBlank(line)" style="position: relative">
              <span>{{ line.quantity }}</span>
              <div class="filter-class" :style="!line.boolDrillEnum ? 'background: #f56c6c' : 'background: #40ed8d'">
                <span v-if="!line.boolDrillEnum">总装</span>
                <span v-else>钻孔</span>
              </div>
            </div>
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

const props = defineProps({
  separateOrderInfo: {
    type: Array,
    default: () => []
  }
})

console.log(props.separateOrderInfo, 'separateOrderInfo')
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
  text-align: center;
}

.separate-table .separate-td-img img {
  width: 95%;
  height: 95%;
  vertical-align: middle;
}
.filter-class {
  color: white;
  font-weight: 100;
  position: absolute;
  top: -29px;
  left: 0px;
  right: 0;
  width: 50px;
  height: 20px;
  font-size: 10px;
  display: flex;
  justify-content: center;
  align-items: center;
}
</style>
