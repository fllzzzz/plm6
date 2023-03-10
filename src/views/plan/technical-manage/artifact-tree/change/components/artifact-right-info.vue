<template>
  <el-card v-if="changeInfo?.length > 1" class="artifact-right-info">
    <div class="head-tip">
      <span>变更构件共 {{ changeInfo?.length }} 种，已处理 2 种 </span>
    </div>
    <div class="sn-list">
      <div class="sn" v-for="(item, index) in changeInfo" :key="index" :style="artifactHandleStatus['UN_HANDLE'].style">
        {{ item.newArtifact?.serialNumber }}
      </div>
    </div>
    <el-divider />
    <div class="foot-tip">
      <div v-for="(item, index) in artifactHandleStatus" :key="index" class="tip-status">
        <div class="illustration" :style="item.style"></div>
        <div class="label">{{ item.L }}</div>
      </div>
    </div>
  </el-card>
</template>

<script setup>
import { artifactHandleStatus } from './common'
import { inject } from 'vue'

const changeInfo = inject('changeInfo')
</script>

<style lang="scss" scoped>
.artifact-right-info {
  height: 100%;

  ::v-deep(.el-divider--horizontal) {
    margin: 20px 0;
  }

  .head-tip {
    font-weight: bold;
    text-align: center;
    margin-bottom: 20px;
  }

  .sn-list {
    display: flex;
    align-items: center;
    flex-wrap: wrap;

    .sn {
      font-size: 14px;
      padding: 5px 10px;
      text-align: center;
      border-radius: 1.5px;
    }
  }

  .foot-tip {
    margin-top: 10px;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-wrap: wrap;
    .tip-status {
      padding: 0 5px;
      display: flex;
      align-items: center;
      justify-content: center;
      margin-bottom: 8px;

      .illustration {
        box-sizing: border-box;
        width: 30px;
        height: 17px;
        margin-right: 8px;
        filter: drop-shadow(rgba(0, 0, 0, 0.15) 0px 2px 1px);
        border-radius: 1.5px;
      }

      .label {
        color: rgb(51, 51, 51);
        font-size: 12px;
      }
    }
  }
}
</style>
