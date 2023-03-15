<template>
  <div class="summary-content" :style="heightStyle">
    <div class="left-con">
      <el-card>
        <div class="box-content">
          <div class="summary-title">
            <span># 构件汇总</span>
          </div>
          <artifact-change-summary-table :artifactCompareList="summaryInfo.artifactList || []" />
        </div>
        <div class="box-content" v-if="summaryInfo?.assembleList?.length">
          <div class="summary-title">
            <span># 部件汇总</span>
          </div>
          <assemble-change-table :assembleCompareList="summaryInfo.assembleList || []" showProcess />
        </div>
        <div class="box-content" v-if="summaryInfo?.partList?.length">
          <div class="summary-title">
            <span># 零件汇总</span>
          </div>
          <machine-part-change-table :partCompareList="summaryInfo.partList || []" showProcess/>
        </div>
      </el-card>
    </div>
    <!-- <div class="right-con"></div> -->
  </div>
</template>

<script setup>
import { inject, defineProps } from 'vue'
import artifactChangeSummaryTable from '../components/artifact-change-summary-table'
import assembleChangeTable from '../components/assemble-change-table'
import machinePartChangeTable from '../components/machine-part-change-table'

defineProps({
  heightStyle: {
    type: String
  }
})

const summaryInfo = inject('summaryInfo')
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}
.summary-content {
  display: flex;

  .left-con {
    flex: 1;
    height: 100%;
    overflow: auto;
  }

  .right-con {
    margin-left: 15px;
    width: 100px;
    height: 100%;
  }
}

.box-content {
  margin-bottom: 20px;
}
.summary-title {
  font-size: 18px;
  color: rgb(51, 51, 51);
  font-weight: bold;
  white-space: nowrap;
  letter-spacing: 0px;
  word-break: normal;
  margin-bottom: 10px;
}
</style>
