<template>
  <el-tooltip class="item" effect="dark" :content="tip" :disabled="!showTooltip" placement="top-start">
    <el-button
      v-permission="permission"
      :loading="downloadLoading"
      :disabled="disabled"
      type="warning"
      icon="el-icon-download"
      size="mini"
      @click.stop="doExport"
    >
      <span v-if="showText" v-text="tip" />
    </el-button>
  </el-tooltip>
</template>
<script>
import { crud } from '@crud/crud'
import { fileDownload } from '@/utils/other'

export default {
  mixins: [crud()],
  props: {
    data: {
      type: null,
      default: undefined
    },
    permission: {
      type: Array,
      required: true
    },
    disabled: {
      type: Boolean,
      default: false
    },
    tip: {
      type: String,
      default: '导出'
    },
    customFn: {
      type: Function,
      default: undefined
    },
    showText: {
      type: Boolean,
      default: false
    },
    showTooltip: {
      type: Boolean,
      default: false
    }
  },
  data() {
    return {
      downloadLoading: false
    }
  },
  methods: {
    async doExport() {
      try {
        this.downloadLoading = true
        if (this.customFn) {
          await fileDownload(this.customFn, this.data)
        } else {
          this.crud.downloadLoading = true
          await this.crud.doExport(this.data)
        }
      } catch (error) {
        console.log('通用导出', error)
      } finally {
        this.downloadLoading = false
        if (this.$isNotBlank(this.customFn)) {
          this.crud.downloadLoading = false
        }
      }
    }
  }
}
</script>
