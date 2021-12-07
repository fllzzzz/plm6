<template>
  <div class="app-container project-state-view-main">
    <div class="view-left">
      <div style="height: 32px"></div>
      <el-card v-for="(item, index) in leftList" :key="index" class="left-item">
        <div class="left-item-title">
          <span class="title">结构</span>
          <span class="unit">单位：kg</span>
        </div>
        <div class="left-item-content">
          <el-progress type="circle" :percentage="60" :stroke-width="8" :width="100" :color="colors">
            <template #default="{ percentage }">
              <span style="font-size: 16px">{{ toFixed(percentage, 2) }}%</span>
            </template>
          </el-progress>
          <div class="left-item-detail">
            <div>清单量：{{ toFixed(4545.154545, DP.COM_WT__KG) }}</div>
            <div>生产量：{{ toFixed(45782.454857, DP.COM_WT__KG) }}</div>
          </div>
        </div>
      </el-card>
    </div>
    <div class="view-center">
      <common-radio-button v-model="projectType" :options="projectComponentTypeEnum.ENUM" type="enum" size="small" />
      <el-descriptions direction="vertical" :column="4" border>
        <el-descriptions-item align="center" label="生产">1000</el-descriptions-item>
        <el-descriptions-item align="center" label="生产率">70%</el-descriptions-item>
        <el-descriptions-item align="center" label="已发运">2000</el-descriptions-item>
        <el-descriptions-item align="center" label="发运率">20%</el-descriptions-item>
      </el-descriptions>
      <common-table show-summary v-loading="tableLoading" :data="[{}, {}]" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="name" :show-overflow-tooltip="true" label="名称">
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="totalQuantity" :show-overflow-tooltip="true" label="总数">
          <template v-slot="scope">
            <span>{{ scope.row.totalQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="producedQuantity" :show-overflow-tooltip="true" label="已生产">
          <template v-slot="scope">
            <span>{{ scope.row.producedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unProducedQuantity" :show-overflow-tooltip="true" label="未生产">
          <template v-slot="scope">
            <span>{{ scope.row.unProducedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="shippedQuantity" :show-overflow-tooltip="true" label="已发运">
          <template v-slot="scope">
            <span>{{ scope.row.shippedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unShipQuantity" :show-overflow-tooltip="true" label="未发运">
          <template v-slot="scope">
            <span>{{ scope.row.unShipQuantity }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <div class="view-right">
      <div style="height: 32px"></div>
      <el-descriptions direction="vertical" :column="3" border>
        <el-descriptions-item align="center" label="开始生产">2021.1.1</el-descriptions-item>
        <el-descriptions-item align="center" label="完成时间">2021.1.31</el-descriptions-item>
        <el-descriptions-item align="center" label="累计耗时">31</el-descriptions-item>
      </el-descriptions>

      <div id="shipMain" class="ship-echarts"></div>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'

import { projectComponentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import useBaseLineEcharts from '@compos/mes/production-manage/use-base-line-echarts'

const colors = [
  { color: '#f56c6c', percentage: 30 },
  { color: '#e6a23c', percentage: 70 },
  { color: '#6f7ad3', percentage: 100 }
]
const leftList = ref([{}, {}, {}, {}])
const projectType = ref(projectComponentTypeEnum.ARTIFACT.V)
const tableLoading = ref(false)

useBaseLineEcharts({ elementId: 'shipMain', title: '累计发运车次', xAxisData: [1, 2, 3, 4, 5, 6, 7] })
</script>

<style lang="scss" scoped>
.project-state-view-main {
  display: flex;
  overflow: auto;

  & > :not(:last-child) {
    margin-right: 15px;
  }

  & > div > :not(:last-child) {
    margin-bottom: 10px;
  }
}

.view-left {
  flex: 0 0 300px;

  .left-item {
    display: flex;
    flex-direction: column;
    width: 100%;

    .left-item-title {
      display: flex;
      justify-content: space-between;
      align-items: center;

      .title {
        display: flex;
        align-items: center;
        font-weight: bold;
        font-size: 16px;
        margin-right: 15px;
        color: #000;
        position: relative;
        padding-left: 10px;
        box-sizing: border-box;

        // &::before {
        //   content: '';
        //   width: 0;
        //   height: 0;
        //   border-top: 5px solid transparent;
        //   border-bottom: 5px solid transparent;
        //   border-left: 5px solid #909399;
        //   position: absolute;
        //   top: 50%;
        //   left: 0;
        //   transform: translateY(-50%);
        // }
      }

      .unit {
        font-size: 14px;
        color: #909399;
      }
    }

    .left-item-content {
      margin-top: 20px;
      display: flex;
      align-items: center;

      .left-item-detail {
        margin-left: 15px;
        font-size: 14px;

        & > :not(:last-child) {
          margin-bottom: 15px;
        }
      }
    }
  }
}

.view-center {
  flex: 1 1 auto;
}

.view-right {
  flex: 0 0 600px;
  .ship-echarts {
    width: 100%;
    height: 350px;
  }
}
</style>

<style>
.project-state-view-main .el-card__body {
  padding: 10px;
}

.project-state-view-main .el-descriptions__content {
  font-size: 25px;
  font-weight: 700;
}
</style>
